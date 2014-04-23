#= require ./utilities
#= require ./config
#= require ./socketHandler
class GameObject # "abstract" because of the missing @
  constructor: (@name, @img, @x, @y) ->
    @rotation = 0; # east, clockwise
    @image = new Image()
    @image.src = img
    @lifeTime = 0

  update: (deltaTime) =>
    @lifeTime++


class @Ship extends GameObject

  getNextCoordinate = (x, y, rotation) ->
    switch rotation
      when 0 then x++ # east
      when 1 then y++ # south
      when 2 then x-- # west
      when 3 then y-- # north
    return { x: x, y: y }

  constructor: (@x,@y) ->
    super "PirateShip", Config.shipImage, x, y
    @actions = []
#    @actions = [@move, @move, @rotateLeft, @rotateLeft,
#                @move, @move, @rotateRight, @rotateRight]
    @handler = new OperationHandler {
      "left": @rotateLeft
      "move": @move
      "right": @rotateRight
    }

  addToQueue: (fun) =>
    @actions.push fun

  rotateRight: () =>
    @addToQueue @rotateRight_

  rotateRight_: () =>
    @rotation = (@rotation+1) % 4

  rotateLeft: () =>
    @addToQueue @rotateLeft_

  rotateLeft_: () =>
    @rotation = (@rotation-1) % 4;
    if @rotation < 0 # unexpected js
      @rotation = 4 + @rotation

  monsterInFront: () =>
    @addToQueue @monsterInFront_

  monsterInFront: () => # TODO kann probleme geben, wenn bewegung in queue...
    next = getNextCoordinate(@x,@y,@rotation)
    for obj in window.grid.objects
      if obj.x == next.x && obj.y == next.y && obj.name == "Buoy"
        return true
    false

  move: () =>
    @addToQueue @move_

  move_: () =>
    coords = getNextCoordinate(@x,@y,@rotation)
    @x = coords.x
    @y = coords.y

  update: (deltaTime) => # called every frame
    if (Config.simulationSpeed > 0 && (@lifeTime % Config.simulationSpeed) != 0) || @actions.length < 1
      super deltaTime
      return

    op = @actions.shift()
    op()
    super deltaTime # update lifeTime

class @Buoy extends GameObject
  constructor: (x,y)->
    super "Buoy", Config.buoyImage, x, y

