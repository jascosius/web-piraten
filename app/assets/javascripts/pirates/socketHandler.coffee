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
      'take'
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

  highlightLine: (line) ->
    if @lastLine?
      window.codeMirror.removeLineClass @lastLine, 'background', 'processedLine'
    else
      # reset everything when simulation starts (again)
      window.codeMirror.removeLineClass(i, 'background', 'processedLine') for i in [0..window.codeMirror.lineCount()]

    window.codeMirror.addLineClass line, 'background', 'processedLine'
    console.log "Highlighted line #{line}"
    @lastLine = line

  clear: () =>
    @operationQueue = []
    window.codeMirror.removeLineClass(i, 'background', 'processedLine') for i in [0..window.codeMirror.lineCount()]

  update: (deltaTime) =>
    if !window.isSimulating then return
    if (Config.simulationSpeed > 0 && (@lifeTime % Config.simulationSpeed) != 0) || @operationQueue.length < 1
      @lifeTime++
      return
    repeat = true

    while repeat
      currentOp = @operationQueue.shift() # Operation instance
      repeat = false

      nextOp = (op) =>
        if !(op in ['line','done']) && @operationQueue.length > 0 && @operationQueue[0].event == 'line'
          repeat = true
        else
          repeat = false

      switch currentOp.event
        when 'left'
          ship.rotateLeft()
        when 'right'
          ship.rotateRight()
        when 'move'
          ship.move()
        when 'put'
          ship.put()
        when 'take'
          ship.take()
        when 'line'
          @highlightLine currentOp.data
        when 'done'
          Utils.log 'Ausführung beendet!'
          window.toggleCodeEditing()
        else
          window.Utils.logError "Invalid event: #{currentOp.event} data: #{currentOp.data}"
      nextOp(currentOp.event)

    @lifeTime++

class @DebugHandler extends ChannelHandler
  logToConsole = (data) ->
    console.log data
  constructor: () ->
    super "debug"
    @channel.bind "console", logToConsole