class @Console

  $console = $ '#console'
  printToConsole = (message, type) ->
    line = $ '<p></p>'
    line.addClass type

    # anti xss
    line.html Utils.escapeHTML(message)
    $console.append line
    console.log message
    $console.scrollTop($console[0].scrollHeight)

  # log a simple message
  @log = (message) ->
    printToConsole message, 'log'

  # log a yellow warning
  @logWarning = (message) ->
    printToConsole message, 'warning'

  # log a red error message
  @logError = (message) ->
    printToConsole message, 'error'

  # delete everything in the console with a fadeOut animation
  @clear = () ->
    $console.children('p').fadeOut 400, () ->
      $(@).remove()