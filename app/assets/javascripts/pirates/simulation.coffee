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

class @Simulation
  @initialize = () ->
    # load elements
    @canvas = document.getElementById "pirateGrid"
    @context = @canvas.getContext "2d"

    Grid.initialize @canvas, 32
    CodeGUI.initialize('codemirror')

    Grid.addObject (new Buoy 5, 4)
    Grid.ship = new Ship 2, 4

    @debugHandler = new DebugHandler()
    @operationHandler = new OperationHandler()

    @isSimulating = false
    @isInEditor = true

    #main loop
    @lastRun = 0
    @stopRedrawing = false
    @showFps = true

    $(document).on 'mousemove', (event) ->
      if !@mouse?
        @mouse = new Coordinate(0,0);
      @mouse.x = event.clientX || event.pageX
      @mouse.y = event.clientY || event.pageY


  @mainLoop = () =>

    @now = new Date().getTime()
    @deltaTime = (@now - @lastRun)/1000
    @lastRun = @now


    Utils.requestAnimFrame @mainLoop
    if(@stopRedrawing)
      return

    # clear screen
    @context.clearRect 0, 0, @context.canvas.width, @context.canvas.height

    if(@showFps)
      @fps = 1/@deltaTime
      @context.fillStyle = "Black"
      @context.font      = "normal 12pt Arial"
      @context.fillText Math.round(@fps)+" fps", 10, 20

    @operationHandler.update();
    Grid.update()
    Grid.draw()



jQuery () -> # use jQuery to wait until DOM is ready

  Simulation.initialize()
  Simulation.mainLoop()

