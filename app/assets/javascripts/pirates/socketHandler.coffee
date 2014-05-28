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
      'left',
      'right',
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
    @channel.bind 'left', (data) =>
      @operationQueue.push (new Operation 'left', data)
    @channel.bind 'right', (data) =>
      @operationQueue.push (new Operation 'right', data)
    @channel.bind 'move', (data) =>
      @operationQueue.push (new Operation 'move', data)
    @channel.bind 'put', (data) =>
      @operationQueue.push (new Operation 'put', data)
    @channel.bind 'take', (data) =>
      @operationQueue.push (new Operation 'take', data)
    @channel.bind 'look', (data) =>
      @operationQueue.push (new Operation 'look', data)
      @operationQueue.push (new Operation 'lookAway', data)
    @channel.bind 'line', (data) =>
      @operationQueue.push (new Operation 'line', data)
    @channel.bind 'output', (data) =>
      @operationQueue.push (new Operation 'output', data)
    @channel.bind 'output_error', (data) =>
      @operationQueue.push (new Operation 'output_error', data)
    @channel.bind 'done', (data) =>
      @operationQueue.push (new Operation 'done', data)
    @channel.bind 'done_error', (data) =>
      @operationQueue.push (new Operation 'done_error', data)
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
        when 'left'
          Grid.ship.rotateLeft()
        when 'right'
          Grid.ship.rotateRight()
        when 'move'
          Grid.ship.move()
        when 'look'
          Grid.ship.look(currentOp.data)
        when 'lookAway'
          Grid.ship.lookAway()
        when 'put'
          Grid.ship.put()
        when 'take'
          Grid.ship.take()
        when 'line'
          CodeGUI.highlightLine currentOp.data
        when 'output'
          Utils.log currentOp.data
        when 'output_error'
          Utils.logError currentOp.data
        when 'done'
          Utils.log 'Ausführung beendet!'
          CodeGUI.toggleCodeEditing()
        when 'done_error'
          Utils.logError currentOp.data
          CodeGUI.toggleCodeEditing()
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