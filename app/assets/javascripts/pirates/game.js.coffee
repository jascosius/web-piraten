#= require websocket_rails/main
#= require codemirror
#= require codemirror/addons/selection/active-line
#= require codemirror/modes/ruby
#= require codemirror/keymaps/sublime
#= require jquery
#= require ./config
#= require ./utilities
#= require ./socketHandler
#= require ./gameObjects
#= require ./grid
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



  @grid.addObject buoy
  @grid.addObject ship

  @debugHandler = new DebugHandler()
  @operationHandler = new @OperationHandler()

  @isSimulating = false

  # Initialize CodeMirror
  @codeMirror = CodeMirror.fromTextArea document.getElementById("codemirror"), {
    lineNumbers: true,
    autofocus: true,
    tabMode: 'spaces',
    enterMode: 'spaces'
    mode: 'ruby',
    theme: 'monokai',
    keymap: 'sublime',
    styleActiveLine: true,
  }
  $('#codeMirror-loading').hide()

  # Click handlers for the buttons

  # disable buttons and lock CodeMirror
  toggleCodeMirrorOption = (option, value, def) ->
    if codeMirror.options[option] != value
      codeMirror.setOption option, value
    else codeMirror.setOption option, def


  toggleCodeEditing = () ->

    $('.code-controls').toggleClass 'hidden'

    #lock CodeMirror
    toggleCodeMirrorOption 'readOnly', true, false
    toggleCodeMirrorOption 'styleActiveLine', false, true
    $(".code-wrapper .CodeMirror").toggleClass 'editing-disabled'
    @isSimulating = !@isSimulating

  $("#runBtn").click () -> # start execution
    toggleCodeEditing()
    webSocket.trigger "code", {code: window.codeMirror.getValue()}
  $("#stopBtn").click () -> # start execution
    operationHandler.clear()
    toggleCodeEditing()

  $("#resetBtn").click () -> # start execution
    operationHandler.clear()
    toggleCodeEditing()

  $("#debugBtn").click () -> # start execution
    toggleCodeEditing()

  ###
    Main Loop
  ###
  lastRun = 0
  window.stopRedrawing = false
  window.showFps = true


  mainLoop = () ->

    now = new Date().getTime()
    deltaTime = (now - lastRun)/1000
    lastRun = now


    Utils.requestAnimFrame mainLoop
    if(window.stopRedrawing)
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

