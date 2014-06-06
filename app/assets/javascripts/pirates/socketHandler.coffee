#= require ./utilities
#= require websocket_rails/main

onOpen = (event) ->
  Utils.log "Verbindung zum Server hergestellt!"

onClose = (event) ->
  Utils.log "Verbindung zum Server verloren, bitte lade die Seite neu!"


onError = (event) ->
  Utils.logError "Fehler bei der Verbindung mit dem Server, bitte lade die Seite neu!"
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
    Utils.logError "Fehler bei der Kommunikation, bitte lade die Seite neu!"
    console.log event


# channel operator for game object operations
class @OperationHandler extends ChannelHandler

  constructor: ()->
    super "operations"
    @lifeTime = 0
    @operationQueue = []
    @operations = [
      'turn',
      'move',
      'put',
      'line',
      'output',
      'output_error',
      'done',
      'done_error'
      'take',
      'look',
      'lookAway'
    ]

    #bind operations for the operations channel
    # manual mode, because only Chrome offers the 'event' object
    @channel.bind 'turn', (data) =>
      @operationQueue.push (new Operation 'turn', data) if Simulation.isSimulating
    @channel.bind 'move', (data) =>
      @operationQueue.push (new Operation 'move', data) if Simulation.isSimulating
    @channel.bind 'put', (data) =>
      @operationQueue.push (new Operation 'put', data) if Simulation.isSimulating
    @channel.bind 'take', (data) =>
      @operationQueue.push (new Operation 'take', data) if Simulation.isSimulating
    @channel.bind 'look', (data) =>
      if Simulation.isSimulating
        @operationQueue.push (new Operation 'look', data)
        @operationQueue.push (new Operation 'lookAway', data)
    @channel.bind 'line', (data) =>
      @operationQueue.push (new Operation 'line', data) if Simulation.isSimulating
    @channel.bind 'output', (data) =>
      @operationQueue.push (new Operation 'output', data) if Simulation.isSimulating
    @channel.bind 'output_error', (data) =>
      @operationQueue.push (new Operation 'output_error', data) if Simulation.isSimulating
    @channel.bind 'done', (data) =>
      @operationQueue.push (new Operation 'done', data) if Simulation.isSimulating
    @channel.bind 'done_error', (data) =>
      @operationQueue.push (new Operation 'done_error', data) if Simulation.isSimulating
    ###for op in @operations
      @channel.bind op, (data) ->
        \#\#\#
          WebSocketRails workaround, because it does not give information
          what the currently triggered event is. Works because event variable is a
          MessageEvent from the native Websocket implementation (only in Chrome)
        \#\#\#
        operation = $.parseJSON(event.data)[0][0]
        @operationQueue.push new Operation(operation, data)
    ###

  clear: () =>
    @operationQueue = []
    CodeGUI.clearHighlighting()

  update: () =>
    if !Simulation.isSimulating then return
    if (Config.simulationSpeed > 0 && (@lifeTime % Config.simulationSpeed) != 0) || @operationQueue.length < 1
      @lifeTime++
      return
    @simulateOperation()

    @lifeTime++

  simulateOperation: () =>
    repeat = true

    while repeat
      currentOp = @operationQueue.shift() # Operation instance
      repeat = false

      switch currentOp.event
        when 'turn'
          Grid.ship.turn(currentOp.data)
        when 'move'
          Grid.ship.move(currentOp.data)
        when 'look'
          Grid.ship.look(currentOp.data)
        when 'lookAway'
          Grid.ship.lookAway()
        when 'put'
          Grid.ship.put(currentOp.data)
        when 'take'
          Grid.ship.take(currentOp.data)
        when 'line'
          CodeGUI.highlightLine currentOp.data
        when 'output'
          Utils.log currentOp.data
        when 'output_error'
          Utils.logError currentOp.data
        when 'done'
          Utils.log 'Ausführung beendet!'
          CodeGUI.toggleCodeEditing()
          @operationQueue = []
        when 'done_error'
          Utils.logError currentOp.data
          CodeGUI.toggleCodeEditing()
          @operationQueue = []
        else
          Utils.logError "Invalid event: #{currentOp.event} data: #{currentOp.data}"

      if !(currentOp.event in ['line','done','done_error']) && @operationQueue.length > 0 && @operationQueue[0].event == 'line'
          repeat = true


class @DebugHandler extends ChannelHandler
  logToConsole = (data) ->
    Utils.log data
  constructor: () ->
    super "debug"
    @channel.bind "console", logToConsole

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
      exit: Simulation.stop
    }
    return _mapping

  addToQueue = (packet) =>
    console.log 'received packet', packet
    @packetQueue.push packet

  @initialize = () ->
    @channel = webSocket.subscribe 'simulation', @onSubscribe, @onSubscribeFail
    @channel.bind 'step', addToQueue
    @lifeTime = 0
    @packetQueue = []
    @usedIDs = []
    @currentId = -1

  onSubscribe: (event) =>
    console.log "Subscribed to channel '#{@channelName}'"

  onSubscribeFail: (event) =>
    Utils.logError "Fehler bei der Kommunikation, bitte lade die Seite neu!"
    console.log event

  @clear = () =>
    @packetQueue = []
    CodeGUI.clearHighlighting()

  @update = () =>
    if !Simulation.isSimulating then return
    if (Config.simulationSpeed > 0 && (@lifeTime % Config.simulationSpeed) != 0) || @packetQueue.length < 1
      @lifeTime++
      return
    simulatePacket()
    @lifeTime++

  validateArray = (name, arr) ->
    throw "#{name} in packet are not an array" unless Array.isArray arr
    throw "#{name} is empty" if arr.length <= 0

  simulateOperation = (packet) =>
    operations = packet.operations
    validateArray "operations", operations

    for op in operations
      if operationMapping()[op.name]?
        operationMapping()[op.name](op.return)
      else
        throw "Packet error: Invalid operation!"
        console.log "received invalid operation", op

  simulateLine = (packet) ->
    line = packet.line || 0
    throw "Packet error: line '#{line}' is not a valid line" unless line > 0
    CodeGUI.highlightLine line

  simulateAllocations = (packet) ->
    allocations = packet.allocations

    validateArray "allocations", allocations

    #TODO Debugger GUI
    for variable in allocations
      for name of variable
        Utils.log "#{name} ist belegt mit #{variable[name]}"

  simulateMessages = (packet) ->
    messages = packet.messages
    validateArray "messages", messages

    for messageObj in messages
      switch messageObj.type
        when 'log' then Utils.log messageObj.message
        when 'warning' then Utils.logWarning messageObj.message
        when 'error' then Utils.logError messageObj.message

  simulatePacket = () =>
    return if @packetQueue.isEmpty

    CodeGUI.clearHighlighting()
    Grid.look = null

    currentPacket = @packetQueue.shift()

    if not currentPacket.id?
      console.log 'Missing Id', currentPacket
      throw 'Packet does not have an id!'

    id = currentPacket.id
    if @currentId < 0
      @currentId = id
    else if id > @currentId
      @usedIDs.push id
    else if id in @usedIDs
      simulatePacket() # ignore packet
      return

    simulateOperation currentPacket if currentPacket.operations?
    simulateLine currentPacket if currentPacket.line
    simulateAllocations currentPacket if currentPacket.allocations?
    simulateMessages currentPacket if currentPacket.messages?
