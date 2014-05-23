#= require ./utilities
#= require websocket_rails/main

onOpen = (event) ->
  Utils.log("Connection established!")
#  console.log("Websocket Connection open!")
#  console.log(event)

onClose = (event) ->
  Utils.log("Connection closed!")
#  console.log(event)

onMessage = (event) ->
  Utils.log("Message received:" + event.data)
#  console.log(event)

onError = (event) ->
  Utils.logError("Connection error!")
#  console.log(event)

sendMessage= (message) ->
  Utils.log("Sent: "+message)
  webSocket.send(message)

# establish connection
window.webSocket = new WebSocketRails 'localhost:3000/websocket'
webSocket.on_open    = onOpen
webSocket.on_close   = onClose
webSocket.on_message = onMessage
webSocket.on_error   = onError


# allows to subscribe to several channels
class ChannelHandler

  constructor: (@channelName) ->
    @channel = webSocket.subscribe(channelName, @onSubscribe, @onSubscribeFail)

  onSubscribe: (event) =>
    console.log "Subscribed to channel '#{ @channelName }'!"
    console.log event

  onSubscribeFail: (event) =>
    console.log "Channel '#{ @channelName }' lost connection"
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
      'done',
      'take',
      'look',
      'unlook'
    ]

    #bind operations for the operations channel
    # manual mode, because only Chrome offers the 'event' object
    @channel.bind 'left', (data) =>
      @operationQueue.push new Operation('left', data)
    @channel.bind 'right', (data) =>
      @operationQueue.push new Operation('right', data)
    @channel.bind 'move', (data) =>
      @operationQueue.push new Operation('move', data)
    @channel.bind 'put', (data) =>
      @operationQueue.push new Operation('put', data)
    @channel.bind 'take', (data) =>
      @operationQueue.push new Operation('take', data)
    @channel.bind 'look', (data) =>
      @operationQueue.push new Operation('look', data)
      @operationQueue.push new Operation('lookAway', data)
    @channel.bind 'line', (data) =>
      @operationQueue.push new Operation('line', data)
    @channel.bind 'done', (data) =>
      @operationQueue.push new Operation('done', data)
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

  update: (deltaTime) =>
    if !window.isSimulating then return
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
          Grid.ship.look()
        when 'lookAway'
          Grid.ship.lookAway()
        when 'put'
          Grid.ship.put()
        when 'take'
          Grid.ship.take()
        when 'line'
          CodeGUI.highlightLine currentOp.data
        when 'done'
          Utils.log 'Ausführung beendet!'
          CodeGUI.toggleCodeEditing()
        else
          Utils.logError "Invalid event: #{currentOp.event} data: #{currentOp.data}"

      if !(currentOp.event in ['line','done']) && @operationQueue.length > 0 && @operationQueue[0].event == 'line'
        repeat = true


class @DebugHandler extends ChannelHandler
  logToConsole = (data) ->
    console.log data
  constructor: () ->
    super "debug"
    @channel.bind "console", logToConsole