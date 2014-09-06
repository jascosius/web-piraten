#= require codemirror
#= require codemirror/addons/selection/active-line
#= require codemirror/keymaps/sublime
#= require codemirror/addons/display/fullscreen.js


class @Simulation

  @initialize = () ->
    # load elements
    @canvas = document.getElementById "pirateGrid"
    @context = @canvas.getContext "2d"

    @speed = Math.round Config.maxSimulationSpeed/2 # overriden by load
    Grid.initialize @canvas, 32
    Grid.loadDefault()
    CodeGUI.initialize 'codemirror'
    SocketHandler.initialize()

    @isInExecutionMode = false
    @isStopped = true
    @isFinished = true

    #main loop
    @lastRun = 0
    @stopRedrawing = false
    @showFps = false

    $(document).on 'mousemove', (event) =>
      if !@mouse?
        @mouse = new Coordinate 0,0
      @mouse.x = event.clientX || event.pageX
      @mouse.y = event.clientY || event.pageY

    $saveCode = $ '#save-code'
    Utils.createSaveDialog $saveCode, 'code', Config.language.fileExtension, () ->
      return CodeGUI.getCode()

    $saveGrid = $ '#save-grid'
    Utils.createSaveDialog $saveGrid, 'world', 'world', () ->
      return JSON.stringify Grid.serialize()

    $('#load-code').click () ->
      Utils.createFileUpload {
          fileExtension: Config.language.fileExtension
          onSuccess: (fileContent) ->
            CodeGUI.setCode fileContent
            # close modal
            $('#serialization-modal').modal('hide')
          onInvalidFile: () ->
            alert "Keine .#{Config.language.fileExtension} Datei!"
      }

    $('#load-grid').click () ->
      Utils.createFileUpload {
        fileExtension: 'world'
        onSuccess: (fileContent) ->
          serialized = JSON.parse fileContent
          Grid.load serialized
          # close modal
          $('#serialization-modal').modal('hide')
        onInvalidFile: () ->
          alert 'Keine .world Datei!'
      }

    $(window).on 'beforeunload', (event) =>
      event = event || window.event
      if localStorage? and not @isInExecutionMode and Config.saveToLocalStorage
        serialized = @serialize()
        delete serialized.vars # don't store watchlist
        localStorage.setItem "simulation.#{Config.language.id}", JSON.stringify serialized

    if localStorage?
      data = localStorage.getItem "simulation.#{Config.language.id}"
      if data?
        @load JSON.parse(data)
        CodeGUI.clearHistory()

  @mainLoop = () =>
    @now = new Date().getTime()
    @deltaTime = (@now - @lastRun)/1000
    @lastRun = @now


    Utils.requestAnimFrame @mainLoop
    if(@stopRedrawing)
      return

    # clear screen
    @context.clearRect 0, 0, @context.canvas.width, @context.canvas.height

    SocketHandler.update()
    Grid.update()
    Grid.draw()

    if(@showFps)
      @fps = 1/@deltaTime
      @context.fillStyle = "Black"
      @context.font      = "normal 12pt Arial"
      @context.fillText Math.round(@fps)+" fps", 10, 20


  @preload = (dependencies, callback) =>
    loaded = 0
    for i in [0...dependencies.length]
      script = dependencies[i]
      $.getScript(script).done(() =>
        loaded += 1
        if loaded >= dependencies.length
           @preloadImages callback
      ).fail (jqxhr, settings, exception) ->
        console.log 'failed to load: ', script
        console.log exception

  @preloadImages = (callback) =>
    loaded = 0
    images = []
    for name, img of Config.images
      images.push img
    for i in [0...images.length]
      currentImage = images[i]
      $('<img />').attr('src', currentImage).load () ->
        loaded += 1
        $(this).appendTo '#imagePreloader'
        if loaded >= images.length
          callback()

  tempStorage = null
  @start = () =>
    throw 'already started' if @isInExecutionMode
    clear()
    CodeGUI.toggleCodeEditing()
    @isInExecutionMode = true
    @isStopped = false
    @isFinished = false
    $('#serialization-trigger').attr 'disabled', 'disabled'

    # store state of the grid
    tempStorage = @serialize()
    SocketHandler.startSimulation tempStorage
    if localStorage? and Config.saveToLocalStorage
      localStorage.setItem "simulation.#{Config.language.id}", JSON.stringify(tempStorage)

  @stop = () =>
    throw 'already stopped' if @isStopped
    throw 'not in simulation mode' unless @isInExecutionMode
    @isStopped = true
    CodeGUI.stop()

  @step = () =>
    throw 'can\'t step through a stopped simulation' unless @isInExecutionMode
    SocketHandler.simulatePacket()

  @resume = () =>
    throw 'Simulation is not paused' unless @isStopped
    @isStopped = false
    CodeGUI.resume()

  @reset = () =>
    throw 'not in simulation mode' unless @isInExecutionMode
    @isInExecutionMode = false
    clear()
    SocketHandler.setStackDeep()
    SocketHandler.stopSimulation()
    @isStopped = true
    @isFinished = true
    $serialization_trigger = $ '#serialization-trigger'
    $serialization_trigger.removeAttr 'disabled'
    CodeGUI.reset()
    Console.clear()
    CodeGUI.WatchList.clearAllocations()

    delete tempStorage.speed # usability
    @load tempStorage
    tempStorage = null

  clear = () ->
    CodeGUI.clearHighlighting()
    SocketHandler.clear()
    Grid.look = null

  @serialize = () ->
    {
      code: CodeGUI.getCode()
      grid: Grid.serialize()
      vars: CodeGUI.WatchList.get()
      speed: @speed
      language: Config.language.id
    }


  @load = (obj) ->
    clear()
    throw 'obj null' unless obj?
    CodeGUI.setCode obj.code if obj.code?

    Grid.load obj.grid if obj.grid?
    if obj.vars?
      CodeGUI.WatchList.clear()
      for key of obj.vars
        CodeGUI.WatchList.addVariable obj.vars[key]

    CodeGUI.setSpeed obj.speed if obj.speed?


