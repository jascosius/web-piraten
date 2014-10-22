class @CodeGUI

  @initialize: (@textAreaId) ->
    @WatchList._initialize()

    # create CodeMirror
    @codeMirror = CodeMirror.fromTextArea document.getElementById(textAreaId), Config.codemirror

    # shows the 'close fullscreen' button once the fullscreen starts
    showCloseFullScreenButton = () =>
      btn = '<button class="btn btn-xs" id="closeFullScreenBtn"><span class="glyphicon glyphicon-resize-small" /></span></button>'
      $('body').append btn
      $btn = $('#closeFullScreenBtn')
      $btn.click () =>
        @codeMirror.setOption("fullScreen", false)
        $btn.remove()

    # register keyboard keys with fullscreen mode
    @codeMirror.setOption("extraKeys", {
      'F11': () => # toggle fullscreen
        isFullScreen = @codeMirror.getOption "fullScreen"
        @codeMirror.setOption "fullScreen", !isFullScreen
        if !isFullScreen
          showCloseFullScreenButton()
        else
          $('#closeFullScreenBtn').remove()
      'Esc': () => # close
        if (@codeMirror.getOption("fullScreen"))
          @codeMirror.setOption("fullScreen", false)
          $('#closeFullScreenBtn').remove()
    })

    # codeMirror event handling
    @codeMirror.on  "blur", () => @isInEditor = false
    @codeMirror.on  "focus", () => @isInEditor = true
    @codeMirror.on 'dblclick', @onDoubleClick

    # search DOM for buttons and set callbacks
    @_$fullScreenBtn = $ '#fullScreenBtn'
    @_$fullScreenBtn.click () =>
      @codeMirror.setOption("fullScreen", true)
      showCloseFullScreenButton()
    @_$fullScreenBtn.tooltip()

    # map codeControls
    @_$runBtn = $ '#runBtn'
    @_$runBtn.click Simulation.start

    @_$resetBtn = $ '#resetBtn'
    @_$resetBtn.click Simulation.reset

    @_$stopBtn = $ '#stopBtn'
    @_$stopBtn.click Simulation.stop

    @_$resumeBtn = $ '#resumeBtn'
    @_$resumeBtn.click Simulation.resume

    @_$runNStopBtn = $ '#runNStopBtn'
    @_$runNStopBtn.click () =>
      Simulation.start()
      Simulation.stop()

    @_$jumpBtn = $ '#jumpBtn'
    @_$jumpBtn.attr 'disabled', 'disabled'
    @_$jumpBtn.click () => # chris' comfortable debugging
      acDeep = SocketHandler.stackDeep
      if acDeep <= 0
        try
          Simulation.step()
        catch
          if Simulation.isFinished
            @toggleStepper()
            @_$resumeBtn.attr 'disabled', 'disabled'
      else while SocketHandler.stackDeep >= acDeep && !Simulation.isFinished
        try
          Simulation.step()
        catch
          if Simulation.isFinished
            @toggleStepper()
            @_$resumeBtn.attr 'disabled', 'disabled'

    @_$stepBtn = $ '#stepBtn'
    @_$stepBtn.attr 'disabled', 'disabled'
    @_$stepBtn.click () =>
      try
        Simulation.step()
      catch
        if Simulation.isFinished
          @toggleStepper()
          @_$resumeBtn.attr 'disabled', 'disabled'

    @_$clearConsoleBtn = $ '#clearConsoleBtn'
    @_$clearConsoleBtn.click () =>
      Console.clear()
      @WatchList.clearAllocations()

    # hide pacman
    $('#codeMirror-loading').hide()

    @_$codeControls = $('.code-controls')
    @_$CodeMirror = $('.code-wrapper .CodeMirror')

    # set maximum code length
    # github.com/marijnh/CodeMirror/issues/821#issuecomment-36967065
    @codeMirror.setOption "maxLength", Config.maxCodeLength
    #@codeMirror.on "beforeChange", @enforceMaxLength

    # initialize execution speed slider (jQuery UI)
    @_$speed = $ '#simulationSpeed'
    $("#speedSlider").slider {
      range: 'min'
      value: Simulation.speed
      min: 0
      max: Config.maxSimulationSpeed
      step: 1
      slide: (event, ui) =>
        @setSpeed(Config.maxSimulationSpeed-ui.value)
    }
    # set initial speed from saves
    @setSpeed Simulation.speed

  # set the speed of the execution and display it with the slider
  # speed is a variable that actually sets the delay between packets
  @setSpeed = (speed) =>
    percentage = (Config.maxSimulationSpeed-speed)/Config.maxSimulationSpeed
    percentage *= 100
    percentage = Math.round percentage
    percentage = Math.max percentage, 1 # no 0%

    # change slider
    $("#speedSlider").slider 'value', Config.maxSimulationSpeed-speed
    @_$speed.html "#{percentage} %"

    Simulation.speed = speed #Config.maxSimulationSpeed-speed

  # double click on variables to add them to the watchlist
  # a double click will automatically select the clicked word
  @onDoubleClick = (event) =>
    return if Simulation.isInExecutionMode

    # get selected word
    selection = event.getSelection().trim() # selected word
    cursor = event.getCursor()
    type = event.getTokenTypeAt(cursor)

    # only variables and single words allowed
    if !type? || type != 'variable' || selection.length < 1 || /\s/.test(selection)
      return

    if @WatchList.contains selection
      # already in watchlist
      return

    # add to watchlist
    @WatchList.addVariable selection

    # animate it with a flying word
    hoveringSelection = $ "<div class='flying cm-variable'><span>#{selection}</span></div>"
    hoveringSelection.css({
      position: 'absolute'
      top:  Simulation.mouse.y
      left: Simulation.mouse.x
      display: 'block'
      visibility: 'hidden'
      opacity: 1
      'z-index': 5000
    })
    hoveringSelection.appendTo 'body'


    # move it to the current position of the mouse (centered around the mouse)
    $span = hoveringSelection.children 'span:first'
    hoveringSelection.css({
      top: Simulation.mouse.y - ($span.height()/2)
      left: Simulation.mouse.x - ($span.width()/2)
      visibility: 'visible'
    })

    # animate flight from the mouse to the watchlist dropdown button
    dropdownToggle = $ '#watchlist-dropdown'

    hoveringSelection.animate({
        top: dropdownToggle.offset().top
        left: dropdownToggle.offset().left
        opacity: 0.001
      },
      {
        duration: 1500
        easing: 'easeOutCubic'
        complete: () =>
          # delete flying object
          hoveringSelection.remove()
          @WatchList.updateQueueSize()
      }
    )

  # helper for toggling codeMirror options
  @toggleSetting: (option, value, def) =>
    if @codeMirror.options[option] != value
      @codeMirror.setOption option, value
    else @codeMirror.setOption option, def

  # get ready for execution mode/reset


  @setReadOnly: (bool) =>
    @codeMirror.setOption 'readOnly', bool
    if bool
      @codeMirror.setOption 'styleActiveLine', false
    else
      @codeMirror.setOption 'styleActiveLine', true


#  @toggleReadOnly = () ->
#    @toggleSetting 'readOnly', true, false          #read only
#    @toggleSetting 'styleActiveLine', false, true   #highlight edit line

  @toggleCodeEditing = () ->
    @_$codeControls.toggleClass 'hidden'            # switch buttons

    #lock CodeMirror
    @_$CodeMirror.toggleClass 'editing-disabled'    #color
    @clearHighlighting()

  # enforce maximum amount of code
  # github.com/marijnh/CodeMirror/issues/821#issuecomment-36967065
#  @enforceMaxLength = (cm, change) ->
#    maxLength = cm.getOption "maxLength"
#    if maxLength and change.update
#      str = change.text.join "\n"
#      console.log 'change', change.update
#      delta = str.length-(cm.indexFromPos(change.to) - cm.indexFromPos(change.from))
#      return true if (delta <= 0)
#      console.log 'getValue from enforceMaxLength'
#      delta = cm.getValue().length+delta-maxLength
#      if delta > 0
#        Console.logError 'Maximale Zeichenanzahl erreicht.'
#        str = str.substr 0, str.length-delta
#        change.update change.from, change.to, str.split("\n")
#    return true

  # get the content CodeMirror
  @getCode = () ->
    @codeMirror.getValue()

  # set the content of CodeMirror
  @setCode = (code) ->
    @codeMirror.setValue code

  # clear editing history (ctrl+z) of the editor
  @clearHistory = () =>
    @codeMirror.getDoc().clearHistory()

  # highlight a line in the editor
  @highlightLine: (line) ->
    line -= 1
    if @lastLine?
      @codeMirror.removeLineClass @lastLine, 'background', 'processedLine'

    @codeMirror.addLineClass line, 'background', 'processedLine'
    @lastLine = line

  # remove highlighting
  @clearHighlighting = () ->
    @codeMirror.removeLineClass(i, 'background', 'processedLine') for i in [0..@codeMirror.lineCount()]

  # get ready for execution, called from Simulation
  @start = () =>
    if @_$stepBtn.attr 'disabled'
      @toggleStepper()

  # reset from execution, called from Simulation
  @reset = () =>
    @resetButtons()
    @toggleCodeEditing()
    @setReadOnly(false)

  # reset the execution control buttons
  @resetButtons = () =>
    #reset buttons
    @_$resumeBtn.hide()
    @_$stopBtn.show()
    @_$stepBtn.attr 'disabled', 'disabled'
    @_$jumpBtn.attr 'disabled', 'disabled'

  # toggle the step, jump and resume button state
  @toggleStepper = () =>
    if @_$stepBtn.attr 'disabled'
      @_$stepBtn.removeAttr 'disabled'
      @_$jumpBtn.removeAttr 'disabled'
      @_$resumeBtn.removeAttr 'disabled'
    else
      @_$stepBtn.attr 'disabled', 'disabled'
      @_$jumpBtn.attr 'disabled', 'disabled'
      @_$resumeBtn.attr 'disabled', 'disabled'

  # pause the execution, called from Simulation
  @stop = () =>
    @_$stopBtn.hide()
    @_$resumeBtn.show()
    @toggleStepper()

    if Simulation.isFinished
      console.log "Finish!"
      @_$resumeBtn.attr 'disabled', 'disabled'
      @toggleStepper()

  # resume the execution, called from Simulation
  @resume = () =>
    @_$resumeBtn.hide()
    @_$stopBtn.show()
    @toggleStepper()


# storing of variables to watch in execution time
class CodeGUI.WatchList

  @_initialize = () ->
    # find in DOM
    @_$watchlist = $ '#watchlist'
    @_$default = $ '#watchlist-default'
    @_$size = $ '#watchlist-size'
    @_$watchlist.on 'click','.watchlist-remove', @onClick
    @_$watchlistDebugger = $ '#watchlistDebugger'
    @_$watchlistDebuggerTbody = @_$watchlistDebugger.find 'table tbody'

  # adds a variable to the watchlist, using a html <ul> list to store the information
  @addVariable = (word) ->
    @_$default.hide()
    @_$watchlist.append "<li>
      <span class='glyphicon glyphicon-remove watchlist-remove' aria-hidden='true'></span>
        #{word}
      </li>"
    @updateQueueSize()

  # remove a word from the watchlist
  @remove = (word) ->
    slideDuration = 300

    @_$watchlist.children("li:contains('#{word}')").remove()
    @updateQueueSize()

    # show the default message if the watchlist is empty
    if @_$size.html() < 1 && @_$default.is ':hidden'
      @_$default.fadeIn({
        duration: slideDuration
        queue: false
      })
      .css('display', 'none')
      .slideDown slideDuration

  # remove all entries from the watchlist
  @clear = () ->
    list = @get()
    for word in list
      @remove word

  # set the small red counter of watched variables next to the watchlist button
  @updateQueueSize = () ->
    watchlist = @get()
    @_$size.text watchlist.length

  # get the current list of watched variables by parsing the html list
  @get = () ->
    watchlist = []
    @_$watchlist.children('li').each () ->
      watchlist.push $(@).text().trim() # read current li content
    return watchlist

  # check if a word is in the watchlist
  @contains = (word) ->
    @_$watchlist.children("li:contains('#{word}')").length > 0

  # the click to delete a variable from the watchlist
  @onClick = (event) =>
    # avoid that bootstrap closes the dropdown
    event.stopPropagation()

    slideDuration = 300
    self = this

    # animate by fading out
    $(event.target).parent().fadeOut({
      duration: slideDuration
      queue: false
      complete: () ->

        # delete after animation
        $(this).remove()
        self.updateQueueSize()

        # show the default message if the watchlist is empty
        if self._$size.html() < 1 && self._$default.is ':hidden'
          self._$default.fadeIn({
            duration: slideDuration
            queue: false
          })
          .css('display', 'none')
          .slideDown slideDuration
    })
    .slideUp slideDuration

  # set a variable allocation while execution, called by SocketHandler
  @setAllocation = (variable, allocation) ->

    # anti xss
    variable = Utils.escapeHTML variable
    allocation = Utils.escapeHTML allocation

    # find the row containing the variable
    $row = @_$watchlistDebuggerTbody.children("tr").filter(() ->
      return $(this).find('td:first').html() is variable
    )

    if $row.length < 1 # row does not exist, create a new one
      $row = $ "<tr><td>#{variable}</td><td></td></tr>"
      @_$watchlistDebuggerTbody.append $row

    # set allocation in the row
    $row.children('td:last').html allocation

    # animate by adding a highlight css class
    $row.children().addClass('highlight').delay(250).removeClass 'highlight', 1000, 'linear'

  # clear the allocation table
  @clearAllocations = () ->
    @_$watchlistDebuggerTbody.children().each () ->
      $(@).fadeOut 1000, () ->
        $(@).remove()