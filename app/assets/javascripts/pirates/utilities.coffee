class @Utils
  $console = $ '#console'
  printToConsole = (message, type) ->
    line = $ '<p></p>'
    line.addClass type
    line.html message
    $console.append line
    console.log message
    $console.scrollTop($console[0].scrollHeight)

  @log: (message) ->
    printToConsole message, 'log'

  @logWarning: (message) ->
    printToConsole message, 'warning'

  @logError: (message) ->
    printToConsole message, 'error'

  @requestAnimFrame: (mainLoop) ->
    window.requestAnimationFrame(mainLoop)       ||
    window.webkitRequestAnimationFrame(mainLoop) ||
    window.mozRequestAnimationFrame(mainLoop)    ||
    window.oRequestAnimationFrame(mainLoop)      ||
    window.msRequestAnimationFrame(mainLoop)     ||
    (mainLoop) ->
      window.setTimeout mainLoop, 1000 / 60


class @Coordinate
  constructor: (@x,@y) ->
    # everything is done thanks to @

class @Operation
  constructor: (@event, @data) ->
