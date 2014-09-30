# abstract class for all objects in the grid, use CoffeeScripts 'extends' and to register new ones
class @GameObject
  @ALL = {} # stores a map with the key being the name and the value being the class

  # accept two types of parameters, first for serialized objects and the seconds for manual creation
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

  # register new gameObject class
  @registerGameObject = (name, clazz) =>
    @ALL[name] = clazz

  # get a GameObject class by name, undefined if name is not registered
  @getGameObjectClass = (name) =>
    @ALL[name]

  # check if another object is the current instance,
  # there can only be one object per cell (except the ship)
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

  # save to JavaScript object
  serialize: () =>
    {
      name: @name
      x: @x
      y: @y
    }

  # called once per frame
  update: () =>
    @lifeTime++

# the ship with methods for each operation
class @Ship extends GameObject
  name = "ship"
  GameObject.ALL[name] = Ship

  # see GameObject.constructor
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.images.ship
      @rotation = serialized.rotation
    else if arguments.length <= 3
      @x = arguments[0]
      @y = arguments[1]
      @isRotating = false
      @rotation = arguments[2] || 0 #optional
      @lastRotation = @rotation
      super name, Config.images.ship, @x, @y
    else throw "invalid ship constructor call"

  # rotate the ship to new position
  turn: (rotation) =>
    @isRotating = true
    @lastRotation = @rotation
    @rotation = rotation


  # let the grid highlight the cell the ship looked at
  look: (coord) =>
    Grid.look = new Coordinate(coord.x, coord.y)

  # save object with rotation, see GameObject.serialize
  serialize: () =>
    obj = super()
    obj.rotation = @rotation
    return obj

  # remove the taken object from the grid
  take: (takenObj) =>
#    if takenObj? and takenObj.name in ['treasure', 'buoy'] # no logic in the client!
      Grid.deleteObject takenObj

  # spawn a gameObject at the position of the ship
  put: (obj) =>
   return unless obj?
   if obj.name == "buoy"
    Grid.addObject (new Buoy obj)
   else if obj.name == "treasure"
     Grid.addObject (new Treasure obj)
   else
     console.log obj
     throw 'invalid object to put'

  # move to new position
  move: (coord) =>
    return unless coord?
    @x = coord.x
    @y = coord.y
    @isMoving = true

  # see GameObject.isEqual
  isEqual: (other) =>
    if !super.isEqual(other) || !other.rotation? || other.rotation == @rotation
      return false
    return true


# see GameObject
class @Buoy extends GameObject
  name = "buoy"
  GameObject.ALL[name] = Buoy
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.images.buoy
    else if arguments.length <= 2
      @x = arguments[0]
      @y = arguments[1]
      super name, Config.images.buoy, @x, @y
    else throw "invalid Buoy constructor call"

# see GameObject
class @Wave extends GameObject
  name = "wave"
  GameObject.ALL[name] = Wave
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.images.wave
    else if arguments.length <= 2
      @x = arguments[0]
      @y = arguments[1]
      super name, Config.images.wave, @x, @y
    else throw "invalid Wave constructor call"

# see GameObject
class @Treasure extends GameObject
  name = "treasure"
  GameObject.ALL[name] = Treasure
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.images.treasure
    else if arguments.length <= 2
      @x = arguments[0]
      @y = arguments[1]
      super name, Config.images.treasure, @x, @y
    else throw "invalid Treasure constructor call"

# see GameObject
class @Monster extends GameObject
  name = "monster"
  GameObject.ALL[name] = Monster
  constructor: () ->
    if arguments.length == 1 # serialized obj
      serialized = arguments[0]
      super serialized, Config.images.monster
    else if arguments.length <= 2
      @x = arguments[0]
      @y = arguments[1]
      super name, Config.images.monster, @x, @y
    else throw "invalid Monster constructor call"
