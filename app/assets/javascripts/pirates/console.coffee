class @Console
  $console = $ '#console'
  printToConsole = (message, type) ->
    line = $ '<p></p>'
    line.addClass type
    line.text message
    $console.append line
    console.log message
    $console.scrollTop($console[0].scrollHeight)

  @log = (message) ->
    printToConsole message, 'log'

  @logWarning = (message) ->
    printToConsole message, 'warning'

  @logError = (message) ->
    printToConsole message, 'error'

  @clear = () ->
    $console.children('p').fadeOut 400, () ->
      $(@).remove()