class @CodeGUI

  @initialize: (@textAreaId, codeMode) ->
    @WatchList._initialize()
    @codeMirror = CodeMirror.fromTextArea document.getElementById(textAreaId), Config.codemirror

    @codeMirror.on  "blur", () => @isInEditor = false
    @codeMirror.on  "focus", () => @isInEditor = true
    @codeMirror.on 'dblclick', @onDoubleClick

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
    @_$jumpDropdown = $ '#jumpDropdown'
    @_$jumpDropdown.attr 'disabled', 'disabled'
    @_$jumpBtn = $ '#jumpBtn'
    @_$jumpBtn.attr 'disabled', 'disabled'
    @_$jumpBtn.click () =>
      acDeep = PacketHandler.stackDeep
      if acDeep <= 0
        try
          Simulation.step()
        catch
          if Simulation.isFinished
            console.log 'nein'
            @toggleStepper()
            @_$resumeBtn.attr 'disabled', 'disabled'
      else while PacketHandler.stackDeep >= acDeep && !Simulation.isFinished
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

    $('#codeMirror-loading').hide()

    @_$codeControls = $('.code-controls')
    @_$CodeMirror = $('.code-wrapper .CodeMirror')

    # github.com/marijnh/CodeMirror/issues/821#issuecomment-36967065
    @codeMirror.setOption "maxLength", Config.maxCodeLength
    @codeMirror.on "beforeChange", @enforceMaxLength

  @onDoubleClick = (event) =>
    return if Simulation.isInExecutionMode
    selection = event.getSelection().trim() # selected word
    cursor = event.getCursor()
    type = event.getTokenTypeAt(cursor)

    # only variables and single words allowed
    if !type? || type != 'variable' || selection.length < 1 || /\s/.test(selection)
      return

    if @WatchList.contains selection
      # already in watchlist
      return

    @WatchList.addVariable selection
    hoveringSelection = $ "<div class='flying cm-variable'><span>#{selection}</span></div>"

    dropdownToggle = $ '#watchlist-dropdown'
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

    $span = hoveringSelection.children 'span:first'
    hoveringSelection.css({
      top: Simulation.mouse.y - ($span.height()/2)
      left: Simulation.mouse.x - ($span.width()/2)
      visibility: 'visible'
    })

    hoveringSelection.animate({
        top: dropdownToggle.offset().top
        left: dropdownToggle.offset().left
        opacity: 0.001
      },
      {
        duration: 1500
        easing: 'easeOutCubic'
        complete: () =>
          hoveringSelection.remove()
          @WatchList.updateQueueSize()
      }
    )

  @toggleSetting: (option, value, def) =>
    if @codeMirror.options[option] != value
      @codeMirror.setOption option, value
    else @codeMirror.setOption option, def


  @toggleCodeEditing = () ->
    @_$codeControls.toggleClass 'hidden'

    #lock CodeMirror
    @toggleSetting 'readOnly', true, false
    @toggleSetting 'styleActiveLine', false, true
    @_$CodeMirror.toggleClass 'editing-disabled'
    @clearHighlighting()

  @enforceMaxLength = (cm, change) ->
    maxLength = cm.getOption "maxLength"
    if maxLength and change.update
      str = change.text.join "\n"
      delta = str.length-(cm.indexFromPos(change.to) - cm.indexFromPos(change.from))
      return true if (delta <= 0)
      delta = cm.getValue().length+delta-maxLength
      if delta > 0
        Console.logError 'Maximale Zeichenanzahl erreicht.'
        str = str.substr 0, str.length-delta
        change.update change.from, change.to, str.split("\n")
    return true


  @getCode = () ->
    @codeMirror.getValue()

  @setCode = (code) ->
    @codeMirror.setValue code

  @highlightLine: (line) ->
    line -= 1
    if @lastLine?
      @codeMirror.removeLineClass @lastLine, 'background', 'processedLine'

    @codeMirror.addLineClass line, 'background', 'processedLine'
    @lastLine = line

  @clearHighlighting = () ->
    @codeMirror.removeLineClass(i, 'background', 'processedLine') for i in [0..@codeMirror.lineCount()]

  @start = () =>
#    Simulation.start()
    if @_$stepBtn.attr 'disabled'
      @toggleStepper()

  @reset = () =>
#    Simulation.reset()
    @resetButtons()
    @toggleCodeEditing()

  @resetButtons = () =>
    #reset buttons
    @_$resumeBtn.hide()
    @_$stopBtn.show()
    @_$stepBtn.attr 'disabled', 'disabled'
#    @_$resumeBtn.attr 'disabled', 'disabled'


  @toggleStepper = () =>
    if @_$stepBtn.attr 'disabled'
      @_$stepBtn.removeAttr 'disabled'
      @_$jumpDropdown.removeAttr 'disabled'
      @_$resumeBtn.removeAttr 'disabled'
    else
      @_$stepBtn.attr 'disabled', 'disabled'
      @_$jumpDropdown.attr 'disabled', 'disabled'
      @_$resumeBtn.attr 'disabled', 'disabled'


  @stop = () =>
    @_$stopBtn.hide()
    @_$resumeBtn.show()
#    @_$stepBtn.removeAttr 'disabled'
    @toggleStepper()

    if Simulation.isFinished
      console.log "Finish!"
      @_$resumeBtn.attr 'disabled', 'disabled'
      @toggleStepper()

  @resume = () =>
    @_$resumeBtn.hide()
    @_$stopBtn.show()
    @toggleStepper()
#    Simulation.resume()


# storing of variables to watch in execution time
class CodeGUI.WatchList

  @_initialize = () ->
    @_$watchlist = $ '#watchlist'
    @_$default = $ '#watchlist-default'
    @_$size = $ '#watchlist-size'
    @_$watchlist.on 'click','.watchlist-remove', @onClick
    @_$watchlistDebugger = $ '#watchlistDebugger'
    @_$watchlistDebuggerTbody = @_$watchlistDebugger.find 'table tbody'

  @addVariable = (word) ->
    @_$default.hide()
    @_$watchlist.append Config.getWatchListRemoveButtonHTML(word)
    @updateQueueSize()

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

  @clear = () ->
    list = @get()
    for word in list
      @remove word

  @updateQueueSize = () ->
    watchlist = @get()
    @_$size.html watchlist.length

  @get = () ->
    watchlist = []
    @_$watchlist.children('li').each () ->
      watchlist.push $(@).text().trim()
    return watchlist

  @contains = (word) ->
    @_$watchlist.children("li:contains('#{word}')").length > 0

  @setAllocation = (variable, allocation) ->
    $row = @_$watchlistDebuggerTbody.children("tr").filter(() ->
      return $(this).find('td:first').text() is variable
    )

    if $row.length < 1 # new row
      $row = $ "<tr><td>#{variable}</td><td>#{allocation}</td></tr>"
      @_$watchlistDebuggerTbody.append $row
    allocation = allocation.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
    $row.children('td:last').html allocation
    $row.children().addClass('highlight').delay(250).removeClass 'highlight', 1000, 'linear'

  @clearAllocations = () ->
    @_$watchlistDebuggerTbody.children().each () ->
      $(@).fadeOut 1000, () ->
        $(@).remove()


  @onClick = (event) =>
    # avoid that bootstrap closes the dropdown
    event.stopPropagation()

    slideDuration = 300
    self = this
    $(event.target).parent().fadeOut({
      duration: slideDuration
      queue: false
      complete: () ->
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