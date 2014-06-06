class @Utils

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
