class @CodeGUI

  @initialize: (@textAreaId) ->
    @WatchList._initialize()
    @codeMirror = CodeMirror.fromTextArea document.getElementById(textAreaId), {
      lineNumbers: true,
      autofocus: true,
      tabMode: 'spaces',
      enterMode: 'spaces'
      mode: 'ruby',
      theme: 'monokai',
      keymap: 'sublime',
      styleActiveLine: true,
    }

    @codeMirror.on  "blur", () => @isInEditor = false
    @codeMirror.on  "focus", () => @isInEditor = true
    @codeMirror.on 'dblclick', @onDoubleClick

    $('#runBtn').click @start
    $('#stopBtn').click @stop
    $('#clearConsoleBtn').click () =>
      Console.clear()
      @WatchList.clearAllocations()

    $('#codeMirror-loading').hide()

    @_$codeControls = $('.code-controls')
    @_$CodeMirror = $('.code-wrapper .CodeMirror')

    # github.com/marijnh/CodeMirror/issues/821#issuecomment-36967065
    @codeMirror.setOption "maxLength", Config.maxCodeLength
    @codeMirror.on "beforeChange", @enforceMaxLength

  @onDoubleClick = (event) =>
    return if Simulation.isSimulating
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
          @WatchList.increment()
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
    Simulation.start()

  @stop = () =>
    console.log "Stop!"
    Simulation.stop()
    Grid.ship.isMove = false
    Grid.ship.isRotate = false


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

  @increment = () ->
    @_$size.html parseInt(@_$size.html())+1

  @get = () ->
    watchlist = []
    @_$watchlist.children('li').each () ->
      watchlist.push $(this).text().trim()
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