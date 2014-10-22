#= require codemirror
#= require codemirror/addons/selection/active-line
#= require codemirror/keymaps/sublime
#= require codemirror/addons/display/fullscreen.js


class @Simulation

  # executed after the site is loaded
  @initialize = () ->
    # load elements
    @canvas = document.getElementById "pirateGrid"
    @context = @canvas.getContext "2d" # just for fps drawing

    @speed = Math.round Config.maxSimulationSpeed/2 # overriden by load

    window.dispatchEvent new Event('beforeSimulationInitialize') # IDE says too many arguments, not correct!

    # initialize modules
    Grid.initialize @canvas, 32
    Grid.loadDefault()
    CodeGUI.initialize 'codemirror' # id of the textarea in HTML
    SocketHandler.initialize()

    # initial state
    @isInExecutionMode = false
    @isStopped = true
    @isFinished = true

    # initalize variables for the mainLoop
    @lastRun = 0
    @stopRedrawing = false
    @showFps = false


    # load from previous sessions (code/grid) when the site loads
    if localStorage?
      data = localStorage.getItem "simulation.#{Config.language.id}"
      if data?
        @load JSON.parse(data)
        CodeGUI.clearHistory()

    ###
      initialize JavaScript event handlers
    ###

    # track each mouse movement on the page for quick access everywhere
    $(document).on 'mousemove', (event) =>
      if !@mouse?
        @mouse = new Coordinate 0,0
      @mouse.x = event.clientX || event.pageX
      @mouse.y = event.clientY || event.pageY

    # initialize load/save dialog for the current language
    # also handles click on buttons within the dialog
    $saveCode = $ '#save-code'
    Utils.createSaveDialog $saveCode, 'code', Config.language.fileExtension, () ->
      return CodeGUI.getCode()

    $saveGrid = $ '#save-grid'
    Utils.createSaveDialog $saveGrid, 'world', 'world', () ->
      return JSON.stringify Grid.serialize()

    # load code button in save/load dialog
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

    # load world button in save/load dialog
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

    # save the state of the site when the user leaves the page
    $(window).on 'beforeunload', (event) =>
      event = event || window.event

      # block saving via Config.saveToLocalStorage=false for debugging
      if localStorage? and not @isInExecutionMode and Config.saveToLocalStorage
        serialized = @serialize()
        delete serialized.vars # don't store watchlist, people will forget to clear them
        # store language depending on the language
        localStorage.setItem "simulation.#{Config.language.id}", JSON.stringify serialized

    window.dispatchEvent new Event('simulationInitialized')

  # called about 60 times per second (depended on the browsers refresh rate)
  @mainLoop = () =>

    # calculate frames per second for debugging
    @now = new Date().getTime()

    # time between the frames
    @deltaTime = (@now - @lastRun)/1000
    @lastRun = @now

    # make sure mainLoop gets called again from the browser
    Utils.requestAnimFrame @mainLoop

    # quick fix against overheating in development,
    # via Simulation.stopRedrawing=true in console
    if(@stopRedrawing)
      return

    # simulate packet
    SocketHandler.update()

    # make sure gameObjects get the updates
    Grid.update()

    # draw changes the canvas
    Grid.draw()

    # draw frames per seconds for debugging purposes, via Simulation.showFps=true in console
    if(@showFps)
      @fps = 1/@deltaTime
      @context.fillStyle = "Black"
      @context.font      = "normal 12pt Arial"
      @context.fillText Math.round(@fps)+" fps", 10, 20

  # loads all javascript dependencies synchronously to maintain order
  @preload = (dependencies, callback) =>
    loaded = 0
    for i in [0...dependencies.length]
      script = dependencies[i]
      $.getScript(script).done(() =>
        loaded += 1
        # begin preloading of images if all scripts are loaded
        if loaded >= dependencies.length
           @preloadImages callback
      ).fail (jqxhr, settings, exception) ->
        console.log 'failed to load: ', script
        console.log exception

  # analog to @preload(), but loads all images in the images object in the config
  # uses a hidden image tags and forces the browser to keep the images cached
  # makes sure that the canvas can be rendered correctly
  # (firefox fails if the image is not loaded at the first draw
  @preloadImages = (callback) =>
    loaded = 0
    images = []
    for name, img of Config.images # convert to array for easy length access
      images.push img

    for i in [0...images.length]
      currentImage = images[i]
      $('<img />').attr('src', currentImage).load () ->
        loaded += 1
        $(this).appendTo '#imagePreloader'
        if loaded >= images.length
          # run the callback function if everything is done
          callback()

  # private scope within Simulation,
  # stores the serialized simulation from before the execution begins
  tempStorage = null

  # start the execution by sending everything it to the server
  @start = () =>
    throw 'already started' if @isInExecutionMode
    canceled = !window.dispatchEvent new CustomEvent('beforeSimulationStart', {
      'cancelable': true
    })
    return if canceled

    # make sure it's in the correct state before executing
    clear()
    CodeGUI.toggleCodeEditing()
    CodeGUI.setReadOnly(true)
    @isInExecutionMode = true
    @isStopped = false
    @isFinished = false

    # it should not be possible to load/save while executing
    $('#serialization-trigger').attr 'disabled', 'disabled'

    # store state of the grid
    tempStorage = @serialize()
    SocketHandler.startSimulation tempStorage
    if localStorage? and Config.saveToLocalStorage
      localStorage.setItem "simulation.#{Config.language.id}", JSON.stringify(tempStorage)

    window.dispatchEvent new Event('simulationStarted')

  # stop execution
  @stop = () =>
    throw 'already stopped' if @isStopped
    throw 'not in simulation mode' unless @isInExecutionMode

    canceled = !window.dispatchEvent new CustomEvent('beforeSimulationStop', {
      'cancelable': true
    })
    return if canceled
    @isStopped = true
    CodeGUI.stop()

    window.dispatchEvent new Event('simulationStopped')


  # manually skip through packets
  @step = () =>
    throw 'can\'t step through a non existent simulation' unless @isInExecutionMode
    canceled = !window.dispatchEvent new CustomEvent('beforeSimulationStep', {
      'cancelable': true
    })
    return if canceled
    SocketHandler.simulatePacket()
    window.dispatchEvent new Event('simulationStepped')

  # resume the execution if it is stopped
  @resume = () =>
    throw 'Simulation is not paused' unless @isStopped
    canceled = !window.dispatchEvent new CustomEvent('beforeSimulationResume', {
      'cancelable': true
    })
    return if canceled
    @isStopped = false
    CodeGUI.resume()
    window.dispatchEvent new Event('simulationResumed')

  # reset the simulation from execution mode back to editing mode
  @reset = () =>
    throw 'not in simulation mode' unless @isInExecutionMode
    canceled = !window.dispatchEvent new CustomEvent('beforeSimulationReset', {
      'cancelable': true
    })
    return if canceled

    @isInExecutionMode = false
    clear()

    # Chris' comfortable debugging
    SocketHandler.setStackDeep()

    # delete old packets
    SocketHandler.stopSimulation()
    @isStopped = true
    @isFinished = true

    # enable save/load button
    $('#serialization-trigger').removeAttr 'disabled'
    CodeGUI.reset()
    Console.clear()
    CodeGUI.WatchList.clearAllocations()

    # ignore saved simulation speed because that can be modified in execution mode
    delete tempStorage.speed # usability

    # load client state from before the execution began to restore grid and such
    @load tempStorage
    tempStorage = null

    window.dispatchEvent new Event('simulationReset')

  # private scope within Simulation
  # makes sure that there are no old packets in the SocketHandler queue
  clear = () ->
    CodeGUI.clearHighlighting()
    SocketHandler.clear()

    # stop highlighting of cells in the grid
    Grid.look = null

  # save the state of the client to a javascript object
  @serialize = () ->
    result = {
      code: CodeGUI.getCode()
      grid: Grid.serialize()
      vars: CodeGUI.WatchList.get()
      speed: @speed
      language: Config.language.id
    }
    event = new CustomEvent('simulationSerialized', {
      'detail': result
    })
    window.dispatchEvent event
    return event['detail']

  # can load a serialized client (see @serialize())
  @load = (obj) ->

    event = new CustomEvent('beforeSimulationLoad', {
      'detail': obj
      'cancelable': true
    })
    canceled = !window.dispatchEvent event
    obj = event['detail']
    return if canceled

    clear()
    throw 'cannot load null' unless obj?

    # load code and world
    CodeGUI.setCode obj.code if obj.code?
    Grid.load obj.grid if obj.grid?

    # load watched variables
    if obj.vars?
      CodeGUI.WatchList.clear()
      for key of obj.vars
        CodeGUI.WatchList.addVariable obj.vars[key]

    # sets the simulation speed slider
    CodeGUI.setSpeed obj.speed if obj.speed?

    window.dispatchEvent new Event('simulationLoaded')


