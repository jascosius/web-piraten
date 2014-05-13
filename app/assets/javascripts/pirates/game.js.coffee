#= require websocket_rails/main
#= require ./config
#= require ./utilities
#= require ./socketHandler
#= require ./gameObjects
#= require ./grid
#= require jquery
#= require codemirror
#= require codemirror/addons/selection/active-line
#= require codemirror/modes/ruby
#= require codemirror/keymaps/sublime
# //require opal
# //require ./opal-parser.min
###
  everything that should be done before the main loop starts
###
jQuery () => # use jQuery to wait until DOM is ready
  # load elements
  canvas = document.getElementById "pirateGrid"
  context = canvas.getContext "2d"

  window.grid = new window.Grid canvas, 32

  @ship = new @Ship 2, 4
  @buoy = new @Buoy 5, 4


  window.debugHandler = new DebugHandler()

  @grid.addObject buoy
  @grid.addObject ship

  @operationHandler = new @OperationHandler()


  # Initialize CodeMirror
  window.onload = () ->
    window.codeMirror = CodeMirror.fromTextArea document.getElementById("codemirror"), {
      lineNumbers: true,
      autofocus: true,
      mode: 'ruby',
      theme: 'monokai',
      keymap: 'sublime',
      styleActiveLine: true
    }

  # Click handlers for demo buttons
  $("#leftBtn").click () ->
    webSocket.trigger "ship.left"
  $("#moveBtn").click () ->
    webSocket.trigger "ship.move"
  $("#rightBtn").click () ->
    webSocket.trigger "ship.right"
  $("#runBtn").click () ->
  #  code = injectShipFunctions window.codeMirror.getValue()
  #  console.log code
  #  console.log eval(Opal.compile(code))
    webSocket.trigger "code", {code: window.codeMirror.getValue()}

  injectShipFunctions = (code) ->
    "def move\n
      `window.ship.move()`\n
    end\n
    def turnLeft\n
      `window.ship.rotateLeft()`\n
    end\n
    def turnRight\n
      `window.ship.rotateRight()`\n
    end\n
    def monsterInFront\n
      `window.ship.monsterInFront()`\n
    end\n
    #{ code }"

  ###
    Main Loop
  ###
  lastRun = 0
  window.simulate = true
  window.showFps = true


  mainLoop = () ->

    now = new Date().getTime()
    deltaTime = (now - lastRun)/1000
    lastRun = now


    Utils.requestAnimFrame mainLoop
    if(!window.simulate)
      return

    # clear screen
    context.clearRect 0, 0, context.canvas.width, context.canvas.height

    if(window.showFps)
      fps = 1/deltaTime
      context.fillStyle = "Black"
      context.font      = "normal 12pt Arial"
      context.fillText Math.round(fps)+" fps", 10, 20

    operationHandler.update(deltaTime);
    grid.update(deltaTime)
    grid.draw()


  mainLoop()

