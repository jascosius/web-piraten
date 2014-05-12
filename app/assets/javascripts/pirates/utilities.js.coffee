class @Utils

  @log: (message) ->
    clientConsole = document.getElementById "console"
    line = document.createElement("p")
    line.style.wordWrap = "break-word"
    line.innerHTML = message
    clientConsole.appendChild(line)
    console.log message

  @logError: (message) ->
    clientConsole = document.getElementById "console"
    line = document.createElement("p")
    line.style.wordWrap = "break-word"
    line.innerHTML = message
    line.style.color = "red"
    clientConsole.appendChild(line)
    console.log message


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
