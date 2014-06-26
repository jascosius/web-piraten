
onOpen = (event) =>
  console.log 'Socket connection ready!'

onClose = (event) =>
  Console.logError "Verbindung zum Server verloren, bitte lade die Seite neu!"


onError = (event) ->
  Console.logError "Fehler bei der Verbindung mit dem Server, bitte lade die Seite neu!"
  console.log event

# establish connection
window.webSocket = new WebSocketRails 'localhost:3000/websocket'
webSocket.on_open = onOpen
webSocket.on_close = onClose
webSocket.on_error = onError


# allows to subscribe to several channels
class ChannelHandler

  constructor: (@channelName) ->
    @channel = webSocket.subscribe channelName, @onSubscribe, @onSubscribeFail

  onSubscribe: (event) =>
    console.log "Subscribed to channel '#{@channelName}'"

  onSubscribeFail: (event) =>
    Console.logError "Fehler bei der Kommunikation, bitte lade die Seite neu!"
    console.log event

class @PacketHandler extends ChannelHandler

  # can't be stored directly because the Grid does not exist at loading  _mapping = null
  operationMapping = () ->
    return _mapping if _mapping? && _mapping not null
    _mapping = {
      put: Grid.ship.put
      take: Grid.ship.take
      move: Grid.ship.move
      turn: Grid.ship.turn
      look: Grid.ship.look
      exit: () ->
        Simulation.isFinished = true
        Simulation.stop()
    }
    return _mapping

  addToQueue = (packet) =>
    return if not Simulation.isInExecutionMode

    if not packet.id?
      console.log 'Missing Id', packet
      throw 'Packet does not have an id!'

    id = packet.id
    if @currentId < 0
      @currentId = id
    else if id in @usedIDs
      # ignore packet
      simulatePacket() if @packetQueue.length >= 1
      return
    else if id is not @currentId # newer package
      @usedIDs.push @currentId
      @currentId = id
    @packetQueue.push packet

  @initialize = () ->
    @channel = webSocket.subscribe 'simulation', @onSubscribe, @onSubscribeFail
    @channel.bind 'step', addToQueue
    @lifeTime = 0
    @packetQueue = []
    @usedIDs = []
    @currentId = -1
    @packetCounter = 0

  onSubscribe: (event) =>
    console.log "Subscribed to channel '#{@channelName}'"

  onSubscribeFail: (event) =>
    Console.logError "Fehler bei der Kommunikation, bitte lade die Seite neu!"
    console.log event

  @clear = () =>
    @packetQueue = []
    @usedIDs.push @currentId

  @update = () =>
    if !Simulation.isInExecutionMode then return
    speed = Simulation.speed
    if Simulation.isStopped or (speed > 0 && (@lifeTime % speed) != 0) or @packetQueue.length < 1
      @lifeTime++
      return
    @simulatePacket()
    @lifeTime++

  validateArray = (name, arr) ->
    throw "#{name} in packet are not an array" unless Array.isArray arr
    throw "#{name} is empty" if arr.length <= 0

  simulateOperation = (packet) =>
    operations = packet.operations
    validateArray "operations", operations
    # TODO animate or not?
    #Grid.blockAnimation = (operations.length > 1)
    for op in operations
      if operationMapping()[op.name]?
        operationMapping()[op.name](op.return)
      else
        console.log "received invalid operation", op
        throw "Packet error: Invalid operation!"

  simulateLine = (packet) ->
    line = packet.line || 0
    throw "Packet error: line '#{line}' is not a valid line" unless line > 0
    CodeGUI.highlightLine line

  simulateAllocations = (packet) ->
    allocations = packet.allocations
    for name of allocations
      CodeGUI.WatchList.setAllocation name, allocations[name]

  simulateMessages = (packet) ->
    messages = packet.messages
    validateArray "messages", messages

    for messageObj in messages
      msg = messageObj.message
      msg = msg.replace /\t/g, '    ' # 4 whitespaces
      msg = msg.replace /\ /g, '&nbsp;'
      switch messageObj.type
        when 'log' then Console.log msg
        when 'warning' then Console.logWarning msg
        when 'error' then Console.logError msg

  @simulatePacket = () =>
    return if @packetQueue.isEmpty

    CodeGUI.clearHighlighting()
    Grid.look = null

    currentPacket = @packetQueue.shift()

    if @currentId is not currentPacket.id
      console.log 'skipped packet that seems to be old', currentPacket
      return
    @packetCounter++

    simulateLine currentPacket if currentPacket.line
    simulateAllocations currentPacket if currentPacket.allocations?
    simulateMessages currentPacket if currentPacket.messages?

    # simulate operations last so that errors in there do not interfere with messages
    simulateOperation currentPacket if currentPacket.operations?
