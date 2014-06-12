#= require ./utilities
#= require ./config
#= require ./socketHandler
class @GameObject
  @ALL = {} # stores a map with the key being the name and the value being the class
  constructor: () ->
    if arguments.length == 2 # (serialized, img)
      serialized = arguments[0]
      @img = arguments[1]
      @name = serialized.name
      @x = serialized.x
      @y = serialized.y
    else if arguments.length == 4 # (name, img, x, y)
      @name = arguments[0]
      @img = arguments[1]
      @x = arguments[2]
      @y = arguments[3]
    else "invalid game object constructor call"
    @image = new Image()
    @image.src = @img
    @lifeTime = 0


  isEqual: (other) =>
    if !other?
      return false
    if !other.name? || other.name != @name
      return false
    if !other.x? || other.x != @x
      return false
    if !other.y? || other.y != @y
      return false
    return true

  serialize: () =>
    {
      name: @name
      x: @x
      y: @y
    }

  update: () =>
    @lifeTime++

class @Ship extends GameObject
  name = "ship"
  GameObject.ALL[name] = Ship

  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.shipImage
      @rotation = serialized.rotation
    else if arguments.length <= 3
      @x = arguments[0]
      @y = arguments[1]
      @rotation = arguments[2] || 0 #optional
      super name, Config.shipImage, @x, @y
    else throw "invalid ship constructor call"

  turn: (rotation) =>
    @rotation = rotation

  look: (coord) =>
    coord2 = new Coordinate()
    coord2.x = coord['x']
    coord2.y = coord['y']
    Grid.look = coord2

  serialize: () =>
    obj = super()
    obj.rotation = @rotation
    return obj

  take: (takenObj) =>
    if takenObj? and takenObj.name in ['treasure', 'buoy']
      Grid.deleteObject takenObj

  put: (obj) =>
   if obj.name == "buoy"
    Grid.addObject (new Buoy obj)
   else if obj.name == "treasure"
     Grid.addObject (new Treasure obj)
   else
     console.log obj
     throw 'invalid object to put'

  move: (coord) =>
    return unless coord?
    @x = coord.x
    @y = coord.y


  isEqual: (other) =>
    if !super.isEqual(other) || !other.rotation? || other.rotation == @rotation
      return false
    return true

class @Buoy extends GameObject
  name = "buoy"
  GameObject.ALL[name] = Buoy
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.buoyImage
    else if arguments.length <= 2
      @x = arguments[0]
      @y = arguments[1]
      super name, Config.buoyImage, @x, @y
    else throw "invalid Buoy constructor call"

class @Wave extends GameObject
  name = "wave"
  GameObject.ALL[name] = Wave
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.waveImage
    else if arguments.length <= 2
      @x = arguments[0]
      @y = arguments[1]
      super name, Config.waveImage, @x, @y
    else throw "invalid Wave constructor call"

class @Treasure extends GameObject
  name = "treasure"
  GameObject.ALL[name] = Treasure
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.treasureImage
    else if arguments.length <= 2
      @x = arguments[0]
      @y = arguments[1]
      super name, Config.treasureImage, @x, @y
    else throw "invalid Treasure constructor call"

class @Monster extends GameObject
  name = "monster"
  GameObject.ALL[name] = Monster
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.monsterImage
    else if arguments.length <= 2
      @x = arguments[0]
      @y = arguments[1]
      super name, Config.monsterImage, @x, @y
    else throw "invalid Monster constructor call"
