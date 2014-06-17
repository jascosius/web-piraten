#= require websocket_rails/main
#= require codemirror
#= require codemirror/addons/selection/active-line
#= require codemirror/modes/ruby
#= require codemirror/keymaps/sublime
#= require jquery.ui.effect.js
#= require jquery.ui.slider
#= require ./config
#= require ./utilities
#= require ./console
#= require ./socketHandler
#= require ./gameObjects
#= require ./codegui
#= require ./grid

class @Simulation
  @initialize = () ->
    # load elements
    @canvas = document.getElementById "pirateGrid"
    @context = @canvas.getContext "2d"

    @speed = Math.round Config.maxSimulationSpeed/2 # overriden by load
    Grid.initialize @canvas, 32
    Grid.loadDefault()
    CodeGUI.initialize 'codemirror'
    PacketHandler.initialize()

    @isSimulating = false

    #main loop
    @lastRun = 0
    @stopRedrawing = false
    @showFps = false

    $(document).on 'mousemove', (event) =>
      if !@mouse?
        @mouse = new Coordinate 0,0
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

    #@operationHandler.update();
    PacketHandler.update()
    Grid.update()
    Grid.draw()

  @preloadImages = (callback) =>
    images = Config.getImagesToPreload()
    loaded = 0
    for i in [0...images.length]
      currentImage = images[i]
      $('<img />').attr('src', currentImage).load () ->
        loaded += 1
        $(this).appendTo '#imagePreloader'
        if loaded >= images.length
          callback()


  @start = () =>
    throw 'already started' if @isSimulating
    clear()
    CodeGUI.toggleCodeEditing()
    @isSimulating = true
    Console.clear()
    CodeGUI.WatchList.clearAllocations()

    webSocket.trigger "simulateGrid", @serialize()

  @stop = () =>
    throw 'already stopped' unless @isSimulating
    @isSimulating = false
    clear()
    CodeGUI.toggleCodeEditing()
    webSocket.trigger 'stop'

  clear = () ->
    CodeGUI.clearHighlighting()
    PacketHandler.clear()
    Grid.look = null

  @serialize = () ->
    {
      code: CodeGUI.getCode()
      grid: Grid.serialize()
      vars: CodeGUI.WatchList.get()
      speed: @speed
    }

  @load = (obj) ->
    clear()
    CodeGUI.setCode obj.code if obj.code?

    Grid.load obj.grid if obj.grid?
    if obj.vars?
      CodeGUI.WatchList.clear()
      for key of obj.vars
        CodeGUI.WatchList.addVariable obj.vars[key]

    console.log 'speed'
    Grid.GridControls.setSpeed obj.speed if obj.speed?
    console.log 'speed2'


jQuery () -> # use jQuery to wait until DOM is ready
  Simulation.preloadImages () -> # firefox won't draw without
    Simulation.initialize()
    Simulation.mainLoop()

