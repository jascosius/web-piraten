class @Utils
  $console = $ '#console'

  @log: (message) ->
    line = $ '<p></p>'
    line.html message
    $console.append line
    console.log message
    $console.scrollTop($console[0].scrollHeight)

  @logError: (message) ->
    line = $ '<p></p>'
    line.addClass 'error'
    line.html message
    $console.append line
    console.log message
    $console.scrollTop($console[0].scrollHeight)


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
