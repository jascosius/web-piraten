#= require websocket_rails/main
#= require codemirror
#= require codemirror/addons/selection/active-line
#= require codemirror/modes/ruby
#= require codemirror/keymaps/sublime
#= require ./config
#= require ./utilities
#= require ./socketHandler
#= require ./gameObjects
#= require ./codegui
#= require ./grid

###
  everything that should be done before the main loop starts
###
jQuery () => # use jQuery to wait until DOM is ready

  mouseListener = (event) ->
    if !mouse?
      window.mouse = new Coordinate(0,0);
    mouse.x = event.clientX || event.pageX
    mouse.y = event.clientY || event.pageY


  $(document).on 'mousemove', mouseListener

  # load elements
  canvas = document.getElementById "pirateGrid"
  context = canvas.getContext "2d"

  Grid.initialize canvas, 32
  CodeGUI.initialize('codemirror')

  Grid.addObject (new @Buoy 5, 4)
  Grid.ship = new @Ship 2, 4

  @debugHandler = new DebugHandler()
  @operationHandler = new @OperationHandler()

  @isSimulating = false
  @isInEditor = true

  @creatObjectFromButton= (x, y) =>
    found = $(".gameObject-controls .btn-success")
    switch found.attr 'id'
        when 'addWave'
          wave = new window.Wave x, y
          Grid.addObject wave
        when 'addTreasure'
          treasure = new window.Treasure x, y
          Grid.addObject treasure
        when 'addMonster'
          monster = new window.Monster x, y
          Grid.addObject monster



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
    Grid.update(deltaTime)
    Grid.draw()


  mainLoop()

