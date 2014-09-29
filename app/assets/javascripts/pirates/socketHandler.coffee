###
  Module for communication between the Rails app and the client
  Uses websocket and json packets which it maps to function in the client
###
class @SocketHandler
  # map name of operations in packets to specific functions in the client,
  # filled by registerOperation
  _mapping = {}

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
    window.dispatchEvent new CustomEvent('socketOpen', {
      'detail': event
    })

  # called when the connection is closed
  onClose = (event) ->
    Console.logError "Verbindung zum Server verloren, bitte lade die Seite neu!"
    window.dispatchEvent new CustomEvent('socketClosed', {
      'detail': event
    })

  # called when the websocket could not send a packet
  onError = (event) ->
    Console.logError "Fehler bei der Verbindung mit dem Server, bitte lade die Seite neu!"
    console.log event

    window.dispatchEvent new CustomEvent('socketError', {
      'detail': event
    })

  @initialize = () ->
    # establish connection
    @webSocket = new WebSocketRails Config.webSocketServer
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

    # register Operations
    # have to be in a function, because Grid.ship changes on every loading of the grid
    @registerOperation 'put', (param) -> Grid.ship.put(param)
    @registerOperation 'take', (param) -> Grid.ship.take(param)
    @registerOperation 'move', (param) -> Grid.ship.move(param)
    @registerOperation 'turn', (param) -> Grid.ship.turn(param)
    @registerOperation 'look', (param) -> Grid.ship.look(param)
    @registerOperation 'exit', () ->
      Simulation.isFinished = true
      Simulation.stop()

  ###
    API Method to allow operations to be added
    uses the name to identity the packet.
    The callback will get executed once a packet with that operation gets simulated
  ###
  @registerOperation = (name, callback) ->
    _mapping[name] = callback

  ###
    returns the function that will get executed when
    a packet with named operation arrives.
    @params name identifier of the function
    @returns undefined if there is no operation with that name
  ###
  @getOperation = (name) ->
    _mapping[name]

  @setStackDeep = () =>
    @stackDeep = 0

  # sends a serialize state of the simulation to rails to start the simulation
  @startSimulation = (serialized) ->
    @webSocket.trigger "simulateGrid", serialized

  # notifies the app if that the execution stopped
  @stopSimulation = () ->
    @webSocket.trigger 'stop'

  # delete everything from the queue
  @clear = () =>
    @packetQueue = []
    @usedIDs.push @currentId

  # called every tick
  @update = () =>
    if !Simulation.isInExecutionMode then return

    canceled = !window.dispatchEvent new CustomEvent('beforeSocketHandlerUpdate', {
      'cancelable': true
    })
    return if canceled

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
      event = new CustomEvent('socketHandlerSimulateOperation', {
        'detail': op
        'cancelable': true
      })
      canceled = !window.dispatchEvent event
      op = event['detail']
      continue if canceled

      if @getOperation(op.name)?
        @getOperation(op.name) op.return
      else
        console.log "received invalid operation", op
        throw "Packet error: Invalid operation!"

  # highlights the current line of an simulation
  simulateLine = (packet) ->
    line = packet.line || 0
    event = new CustomEvent('socketHandlerSimulateLine', {
      'detail': line
      'cancelable': true
    })
    canceled = !window.dispatchEvent event
    line = event['detail']
    return if canceled
    throw "Packet error: line '#{line}' is not a valid line" unless line > 0
    CodeGUI.highlightLine line

  # show variable allocation
  simulateAllocations = (packet) ->
    allocations = packet.allocations
    event = new CustomEvent('socketHandlerSimulateAllocations', {
      'detail': allocations
      'cancelable': true
    })
    canceled = !window.dispatchEvent event
    allocations = event['detail']
    return if canceled
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
      'cancelable': true
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


