#= require websocket_rails/main
#= require codemirror
#= require codemirror/addons/selection/active-line
#= require codemirror/modes/ruby
#= require codemirror/keymaps/sublime
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
  @isInEditor = true

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

  mouseListener = (event) ->
    if !mouse?
      window.mouse = new Coordinate(0,0);
    mouse.x = event.clientX || event.pageX
    mouse.y = event.clientY || event.pageY

  document.addEventListener 'mousemove',mouseListener ,false


  @codeMirror.on "blur", () -> @isInEditor = false

  @codeMirror.on "focus", () -> @isInEditor = true
#
#  activeCodeControls = $('.code-controls:visible:first-child')
#  activeCodeControls.popover({
#    placement: 'right',
#    trigger: 'manual',
#    title: 'Beobachtete Variablen'
#    content: () -> $('#watchlist').html()
#    html: true,
#  })

  @getWatchList = () ->
    watchlist = []
    $("#watchlist li").each () ->
      watchlist.push $(this).text().trim()
    return watchlist


  updateQueueSize = () ->
    watchlist = @getWatchList()
    $('#watchlist-size').html watchlist.length

  $.extend($.easing, # just for a nicer visualization, from jQuery UI
    {
      easeOutCubic: (x, t, b, c, d) ->
        c*((t=t/d-1)*t*t + 1) + b
    }
  )

  @codeMirror.on 'dblclick', (event) =>
    selection = event.getSelection().trim() # selected word
    cursor = event.getCursor()
    type = event.getTokenTypeAt(cursor)
    if selection == '' || !type? || type != 'variable'
      console.log 'Das geklickte Wort kann keine Variable sein'
    else
      if $("#watchlist li:contains('#{selection}')").length > 0
        # already in watchlist
      else
        $('#watchlist').append "<li><span class='glyphicon glyphicon-remove watchlist-remove' aria-hidden='true'></span> #{selection}</li>"
        hoveringSelection = $ "<div class='flying cm-variable'>#{selection}</div>"

        dropdownToggle = $ '#watchlist-dropdown'
        hoveringSelection.css({
          position: 'absolute'
          top:  window.mouse.y
          left: window.mouse.x
          display: 'block'
          opacity: 1
        })
        .appendTo('body')
        .animate({
            top: dropdownToggle.offset().top
            left: dropdownToggle.offset().left
            opacity: 0.0
          },
          {
            duration: 1500
            easing: 'easeOutCubic'
            complete: () ->
              hoveringSelection.remove()
              updateQueueSize()
          }
        )

  # Click handlers for the buttons
  $('#watchlist').on 'click','.watchlist-remove', (event) ->
    $(event.target).parent().remove()
    console.log 'removed element from watch list'
    #TODO WatchList Button animieren
    updateQueueSize()




  # disable buttons and lock CodeMirror
  toggleCodeMirrorOption = (option, value, def) ->
    if codeMirror.options[option] != value
      codeMirror.setOption option, value
    else codeMirror.setOption option, def


  window.toggleCodeEditing = () ->
    $('.code-controls').toggleClass 'hidden'

    #lock CodeMirror
    toggleCodeMirrorOption 'readOnly', true, false
    toggleCodeMirrorOption 'styleActiveLine', false, true
    $('.code-wrapper .CodeMirror').toggleClass 'editing-disabled'
    @isSimulating = !@isSimulating

  $("#runBtn").click () -> # start execution
    window.toggleCodeEditing()
    webSocket.trigger "code", {code: window.codeMirror.getValue()}
  $("#stopBtn").click () -> # start execution
    operationHandler.clear()
    window.toggleCodeEditing()

  $("#resetBtn").click () -> # start execution
    operationHandler.clear()
    window.toggleCodeEditing()

#  $("#debugBtn").click () -> # start execution
#    window.toggleCodeEditing()

  $(".gameObject-controls button").click () ->
    $('.gameObject-controls button').removeClass "btn-success"
    $(this).addClass "btn-success"


  @onKeyDown= (event) =>
    if !@isInEditor
      switch event.keyCode
        when 49, 97
          $(".gameObject-controls button").removeClass "btn-success"
          $("#addTreasure").addClass "btn-success"
        when 50, 98
          $(".gameObject-controls button").removeClass "btn-success"
          $("#addMonster").addClass "btn-success"
        when 51, 99
          $(".gameObject-controls button").removeClass "btn-success"
          $("#addWave").addClass "btn-success"
        when 27
          $(".gameObject-controls button").removeClass "btn-success"





  @window.addEventListener "keydown", onKeyDown, false

  @creatObjectFromButton= (x, y) =>
    found = $(".gameObject-controls .btn-success")
    switch found.attr 'id'
        when 'addWave'
          wave = new window.Wave x, y
          window.grid.addObject wave
        when 'addTreasure'
          treasure = new window.Treasure x, y
          window.grid.addObject treasure
        when 'addMonster'
          monster = new window.Monster x, y
          window.grid.addObject monster



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

