#= require ./utilities
#= require ./config
#= require ./socketHandler
class GameObject # "abstract" because of the missing @
  constructor: (@name, @img, @x, @y) ->
    @rotation = 0; # east, clockwise
    @image = new Image()
    @image.src = img
    @lifeTime = 0

  serialize: () =>
    {
    name: @name
    x: @x
    y: @y
    }

  update: () =>
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

  rotateRight: () =>
    @rotation = (@rotation+1) % 4

  rotateLeft: () =>
    @rotation = (@rotation-1) % 4;
    if @rotation < 0 # unexpected js
      @rotation = 4 + @rotation

  look: () =>
    Grid.look = getNextCoordinate @x, @y, @rotation

  lookAway: () =>
    Grid.look = null

  serialize: () =>
    obj = super()
    obj.rotation = @rotation
    return obj

  take: () =>
    obj = Grid.isSomethingOnPosition @x, @y
    if obj.name == "Treasure"
      Grid.deleteObject obj
      console.log "Gold eingesammelt"
    else
      console.log "hier ist nichts zu holen"

  put: () =>
   if Grid.isSomethingOnPosition(@x, @y) != false
    Utils.logError "hier ist kein Platz mehr für eine Boje"
   else
    Grid.addObject (new Buoy @x, @y)

  move: () =>
    coords = getNextCoordinate @x, @y, @rotation
    @x = coords.x
    @y = coords.y


class @Buoy extends GameObject
  constructor: (x,y)->
    super "Buoy", Config.buoyImage, x, y

class @Wave extends GameObject
  constructor: (x,y)->
    super "Wave", Config.waveImage, x, y

class @Treasure extends GameObject
  constructor: (x,y)->
    super "Treasure", Config.treasureImage, x, y

class @Monster extends GameObject
  constructor: (x,y)->
    super "Monster", Config.monsterImage, x, y
