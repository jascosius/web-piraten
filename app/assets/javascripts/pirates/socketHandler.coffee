###
  Module for communication between the Rails app and the client
  Uses websocket and json datapacket which it also maps to function in the client
###
class @SocketHandler
  # map name of operations in packets to specific functions in the client
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

  # push an incoming packet into the packet queue
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

  # networking error and success callbacks
  onOpen = (event) ->
    console.log 'Socket connection ready!'
    console.log event

  onClose = (event) ->
    Console.logError "Verbindung zum Server verloren, bitte lade die Seite neu!"


  onError = (event) ->
    Console.logError "Fehler bei der Verbindung mit dem Server, bitte lade die Seite neu!"
    console.log event

  @initialize = () ->
    # establish connection
    @webSocket = new WebSocketRails "#{window.location.host}/websocket"
    @webSocket.on_open = onOpen
    @webSocket.on_close = onClose
    @webSocket.on_error = onError
    @webSocket._conn.on_close = onClose
    @webSocket._conn.on_error = onError
    @webSocket._conn.on_open = onOpen

    # listen to step event which includes all packages
    @webSocket.bind 'step', addToQueue

    # initialize member for the SocketHandler scope
    @lifeTime = 0
    @packetQueue = []
    @usedIDs = []
    @currentId = -1
    @packetCounter = 0
    @stackDeep = 0

  @setStackDeep = () =>
    @stackDeep = 0

  # sends a serialize state of the simulation to rails to start the simulation
  @startSimulation = (serialized) ->
    @webSocket.trigger "simulateGrid", serialized

  # notifies the app if that the execution stopped
  @stopSimulation = () ->
    @webSocket.trigger 'stop'

  @clear = () =>
    @packetQueue = []
    @usedIDs.push @currentId

  # called every tick
  @update = () =>
    if !Simulation.isInExecutionMode then return

    window.dispatchEvent new Event('beforeSocketHandlerUpdate')
    speed = Simulation.speed
    if Simulation.isStopped or (speed > 0 && (@lifeTime % speed) != 0) or @packetQueue.length < 1
      @lifeTime++
      return
    @simulatePacket()
    @lifeTime++
    window.dispatchEvent new Event('socketHandlerUpdated')

  validateArray = (name, arr) ->
    throw "#{name} in packet are not an array" unless Array.isArray arr
    throw "#{name} is empty" if arr.length <= 0

  # simulate the operations part of each packet
  simulateOperation = (packet) =>
    operations = packet.operations
    validateArray "operations", operations

    for op in operations
      # allows more message types
      event = new CustomEvent('socketHandlerSimulateSimulateOperation', {
        'detail': op
        'cancelable': true
      })
      canceled = !window.dispatchEvent event
      op = event['detail']
      continue if canceled

      if operationMapping()[op.name]?
        operationMapping()[op.name](op.return)
      else
        console.log "received invalid operation", op
        throw "Packet error: Invalid operation!"

  # highlights the current line of an simulation
  simulateLine = (packet) ->
    line = packet.line || 0
    throw "Packet error: line '#{line}' is not a valid line" unless line > 0
    CodeGUI.highlightLine line

  # show variable allocation
  simulateAllocations = (packet) ->
    allocations = packet.allocations
    for name of allocations
      CodeGUI.WatchList.setAllocation name, allocations[name]

  # simulate console output
  simulateMessages = (packet) ->
    messages = packet.messages
    validateArray "messages", messages

    for messageObj in messages
      msg = messageObj.message

      # allows more message types via event handling
      event = new CustomEvent('socketHandlerSimulateMessage', {
        'detail': messageObj
        'cancelable': true
      })
      canceled = !window.dispatchEvent event
      messageObj = event['detail']
      continue if canceled

      switch messageObj.type
          when 'log' then Console.log msg
          when 'warning' then Console.logWarning msg
          when 'error' then Console.logError msg

  # takes the first packet in queue and simulates each part of it.
  @simulatePacket = () =>
    return if @packetQueue.isEmpty

    CodeGUI.clearHighlighting()
    Grid.look = null # cell highlighting of look operations

    # get first in queue
    currentPacket = @packetQueue.shift()

    # do not allow packets from old execution that might be from an old execution
    if !currentPacket or @currentId is not currentPacket.id
      console.log 'skipped packet that seems to be old or null', currentPacket
      return
    @packetCounter++

    event = new CustomEvent('socketHandlerSimulatePacket', {
      'detail': currentPacket
    })
    window.dispatchEvent event
    currentPacket = event['detail']


    simulateLine currentPacket if currentPacket.line
    simulateAllocations currentPacket if currentPacket.allocations?
    simulateMessages currentPacket if currentPacket.messages?

    # simulate operations last so that errors in there do not interfere with messages
    simulateOperation currentPacket if currentPacket.operations?
    breakPoint currentPacket if currentPacket.break?

  # chris comfortable debugging
  breakPoint = (packet) =>
    breaks = packet.break
    for br in breaks
      switch br.type
        when 'point' then Simulation.stop()
        when 'up'
          if @stackDeep >= 1
            @stackDeep -= 1
          else
            @stackDeep = 0
        when 'down'
          @stackDeep += 1


