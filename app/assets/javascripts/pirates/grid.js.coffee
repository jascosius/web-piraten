class @Grid
  @initialize = (@canvas) ->
    Grid.GridControls._initialize()
    @size = 32 # pixel per cell
    @ctx = canvas.getContext "2d"
    @canvasWidth = @ctx.canvas.width
    @canvasHeight = @ctx.canvas.height

    @defaultData = $.parseJSON $('#gridData').html()

    @activeCell = null
    @look = null
    @mousePosition = null

    $canvas = $(@canvas)
    $canvas.on 'mousedown', this.onClick
    $canvas.on 'mousemove', this.onMouseMove
    $canvas.on 'mouseout', this.onMouseOut
    $canvas.on 'contextmenu', this.onContextmenu
    $canvas.on 'mouseup', this.onMouseUp
    $canvas.on 'selectstart', () -> return false

    @ANGLE = (Math.PI/180)

    @mousePressedOnShip = false
    @mousePressed = false
    @_smoothingStep = 0 #TODO

  @loadDefault = () =>
    @load @defaultData

  @load = (obj) =>
    @gridWidth = obj.width || 21
    @gridHeight = obj.height || 10
    @size = obj.size || 32

    s = obj.ship
    @ship = new Ship s.x, s.y, s.rotation


    @objects = []
    for o in obj.objects
      if GameObject.ALL[o.name]?
        @addObject new GameObject.ALL[o.name](o)
      else
        throw "Invalid GameObject name #{o.name}!"



  @addObject = (obj) ->
    @objects.push(obj)

  @deleteObjectWithIndex = (index) ->
    if index != false
      @objects.splice index, 1

  @deleteObject = (obj) =>
    if !obj?
      throw 'object to delete is undefined!'
    found = false
    newObjects = []
    for i in [0...@objects.length]
      current = @objects[i]
      unless current.isEqual obj
        newObjects.push current
      else
        found = true
    @objects = newObjects
    if not found
      console.log 'unknown object', obj
      throw 'could not delete object from grid'

  @isInCanvas = (coords) =>
    coords.x >= 0 && coords.y >= 0 && coords.x < @canvasWidth && coords.y < @canvasHeight

  @contains = (coords) => # is in grid, pixel
    @isInCanvas(coords) && coords.x <= @gridWidth*@size &&
      coords.y <= @gridHeight*@size

  @getGridCoordinates = (coords) ->
    new Coordinate Math.floor(coords.x/@size), Math.floor((coords.y-2)/@size) #because the two pix thick line -2

  @getCanvasCoordinates = (coords) ->
    new Coordinate Math.floor(coords.x*@size), Math.floor(coords.y*@size)

  @getMousePos = (mouseEvent) =>
    rect = @canvas.getBoundingClientRect()
    new Coordinate mouseEvent.clientX - rect.left, mouseEvent.clientY - rect.top

  # relative to canvas
  @getCellRect = (coords) ->
    x = coords.x*@size
    y = coords.y*@size
    { x1: x, y1: y, x2: @size, y2: @size }

  @onClick = (event) =>
    if !Simulation.isSimulating
      mousPos = @getMousePos event
      pos = @getGridCoordinates mousPos
      objOnPos = @isSomethingOnPosition pos.x, pos.y
      if !(@ship.x == pos.x && @ship.y == pos.y)
        @mousePressed = event.which
      if @ship.x == pos.x && @ship.y == pos.y && event.which == 1
        @mousePressedOnShip = true
      else
        if @contains(mousPos) && objOnPos == false && event.which == 1
          @GridControls.creatObjectFromButton pos.x, pos.y
      if event.which == 3 && @ship.x == pos.x && @ship.y == pos.y
        if @ship.rotation <= 0
          @ship.rotation = 3
        else
          @ship.rotation--
      else if event.which == 3 && objOnPos != false
        @deleteObjectWithIndex @objects.indexOf(objOnPos)


  @onMouseUp = (event) =>
    @mousePressed = false
    coords = @getGridCoordinates @getMousePos(event)
    x = coords.x
    y = coords.y
    if @mousePressedOnShip && Simulation.isSimulating == false
      if !@contains(@getMousePos(event))
        x = coords.x
        y = coords.y
        if x >= @gridWidth
          x = @gridWidth - 1
        if y >= @gridHeight
          y = @gridHeight-1
      @ship.x = x
      @ship.y = y

    @mousePressedOnShip = false

  @onContextmenu = (event) =>
    event.preventDefault()

  @onMouseMove = (event) =>
    @activeCell = null
    pos = @getMousePos(event)
    @mousePosition = pos
    if @contains(pos)
      pos = @getGridCoordinates pos
      @activeCell = pos
    mousPos = @getMousePos event
    pos = @getGridCoordinates mousPos
    objOnPos = @isSomethingOnPosition pos.x, pos.y
    if !Simulation.isSimulating && objOnPos == false && @mousePressed == 1 && @contains(@getMousePos(event))
      @GridControls.creatObjectFromButton pos.x, pos.y
    else if @mousePressed == 3 && objOnPos != false
      @deleteObjectWithIndex @objects.indexOf(objOnPos)


  @onMouseOut = (event) =>
    @mousePressed = false
    @activeCell = null
    if @mousePressedOnShip
      coords = @getGridCoordinates(@getMousePos(event))
      x = coords.x
      y = coords.y
      if x < 0
        x = 0
      else if x >= @gridWidth
        x = @gridWidth - 1

      if y < 0
        y = 0
      else if y >= @gridHeight
        y = @gridHeight-1

      @ship.x = x
      @ship.y = y
    @mousePressedOnShip = false

  @isSomethingOnPosition = (x, y) =>
    for obj in @objects
      if obj.x == x && obj.y == y
        return obj
    false

  @serialize = () =>
    sendObjects = []
    for gameObject in @objects
      sendObjects.push gameObject.serialize()

    sendShip =  @ship.serialize()

    {
      width: @gridWidth
      height: @gridHeight
      objects: sendObjects
      ship: sendShip
    }

  @update = () ->
    for gameObject in @objects
      gameObject.update()
    @ship.update()

  @draw = () =>
    @ctx.save()

    # draw horizontal and vertical lines
    @_drawCells Config.lineColor
    @_drawCells Config.lineColor, true

    @_drawObjects()
    @_drawShip()

    @_highlightCell @activeCell, Config.cellHightlighting.hovered if @activeCell
    @_highlightCell @look, Config.cellHightlighting.look if @look

  @_drawCells = (strokeStyle, vertical) ->
    @ctx.save()
    vertical ||= false
    coords = new Coordinate 0,0 # in pixel

    if vertical
      while(@contains(coords))
        @_drawLine coords.x, 0, coords.x, @gridHeight*@size, 1, strokeStyle
        coords.x += @size
    else
      while(@contains(coords))
        @_drawLine 0, coords.y, @gridWidth*@size, coords.y, 1, strokeStyle
        coords.y += @size

    @ctx.restore()

  @_highlightCell = (cell, fillStyle) ->
    @ctx.save()
    rect = @getCellRect cell

    @ctx.beginPath()
    @ctx.rect rect.x1, rect.y1, rect.x2, rect.y2
    @ctx.fillStyle = fillStyle
    @ctx.fill()
    @ctx.restore()

  @_drawObjects = () ->
    for obj in @objects
      @ctx.save()
      scaleX = @size/obj.image.width
      scaleY = @size/obj.image.height
      posx = obj.x*@size + Math.floor(obj.image.width*scaleX/2)
      posy = obj.y*@size + Math.floor(obj.image.height*scaleY/2)
      @ctx.translate(posx, posy)

      @ctx.scale scaleX, scaleY
      @ctx.drawImage obj.image, -Math.floor(obj.image.width/2), -Math.floor(obj.image.height/2)
      @ctx.restore()

  @_drawShip = () ->
    @ctx.save()
    if @mousePressedOnShip # drag and drop
      @ctx.translate @mousePosition.x, @mousePosition.y
      @ctx.rotate(@ship.rotation * 90 * @ANGLE)

      @ctx.scale @size/@ship.image.width, @size/@ship.image.height
      @ctx.drawImage(@ship.image, -Math.floor(@ship.image.width/2), -Math.floor(@ship.image.height/2))
    else
      widthFactor = @size/@ship.image.width
      heightFactor = @size/@ship.image.height
      newWidth = widthFactor*@ship.image.width
      newHeight = heightFactor*@ship.image.height

      cellCenter = {
        x: @ship.x*@size + (Math.floor(newWidth/2))
        y: @ship.y*@size + Math.floor(newHeight/2)
      }

      if !@_lastPacket?
        @_lastPacket = PacketHandler.packetCounter

      if @_lastPacket != PacketHandler.packetCounter # old packet we are working on
        @_lastPacket = PacketHandler.packetCounter
        @_smoothingStep = 0

      cellCenter = @_smoothShipMovement(cellCenter)

      @ctx.translate cellCenter.x, cellCenter.y
      @ctx.scale widthFactor, heightFactor

      @_smoothShipRotation()

      # uncomment if the ship image does not show a clear rotation
      #if @ship.rotation == 2
      #  @ctx.scale 1, -1 # flip

      # the context is in the center of the image
      @ctx.drawImage(
        @ship.image,
        -Math.floor(@ship.image.width/2),
        -Math.floor(@ship.image.height/2)
      )

    @ctx.restore()

  @_smoothShipMovement = (cellCenter) ->
    unless @ship.isMoving
      @_smoothingStep = 0
      return cellCenter

    axis = -1 #vertical
    direction = -1 # increase ship coordinate

    if @ship.rotation % 2 is 0 # 0 or 2
      axis = 1 #horizontal
    if @ship.rotation in [0,1] # 1 or 3
      direction = 1 # decrease ship coordinate
    speed = Simulation.speed
    speed = Math.max speed, 0.00001
    pxPerFrame = @size/speed

    shipAxis = 'x' #horizontal
    if axis < 0
      shipAxis = 'y' # vertical

    cellCenter[shipAxis] = (@ship[shipAxis] - direction)*@size+(@size*0.5)
    cellCenter[shipAxis] += pxPerFrame*@_smoothingStep*direction

    @_smoothingStep++
    # check if smoothing is/should be done
    if @_smoothingStep >= speed-1
      @ship.isMoving = false

    return cellCenter

  @_smoothShipRotation = () ->
    if !@_smoothingRotationStep?
      @_smoothingRotationStep = 0

    # smooth rotation
    if @ship.isRotating and @ship.rotation != @ship.lastRotation
      @ctx.rotate @ship.lastRotation*90*@ANGLE
      diff = @ship.rotation-@ship.lastRotation

      # pseudo mudulo to avoid 270° rotations at overflows
      if diff > 2 then diff = -1
      else if diff < -2 then diff = 1
      speed = Simulation.speed
      speed = Math.max speed, 0.00001
      @ctx.rotate @_smoothingRotationStep * diff * 90 * @ANGLE / speed
      @_smoothingRotationStep++

      # animation should be done
      if @_smoothingRotationStep >= Simulation.speed-1
        @ship.isRotating = false

    else # static ship
      @ctx.rotate @ship.rotation*90*@ANGLE
      @ship.isRotating = false
      @_smoothingRotationStep = 0

  @_drawLine = (x1, y1, x2, y2, width, strokeStyle) ->
    newX1 = Math.min x1, x2
    newY1 = Math.min y1, y2
    newX2 = Math.max x1, x2
    newY2 = Math.max y1, y2

    @ctx.save()
    @ctx.beginPath()

    # width = 1 fix, should not be 2 pixel thick
    if width <= 1
      newX1 += 0.5
      newY1 += 0.5
      newX2 += 0.5
      newY2 += 0.5

    @ctx.moveTo newX1,newY1
    @ctx.lineTo newX2,newY2
    @ctx.closePath()
    @ctx.lineWidth = width
    @ctx.strokeStyle = strokeStyle
    @ctx.stroke()

    @ctx.restore()


# Controls the buttons above the grid
class Grid.GridControls
  @_initialize = () ->
    $(document).on "keydown", @onKeyDown

    #preload queries for optimization
    @_$buttons = $ ".gameObject-controls button"
    @_$buttons.click @onClick
    @_$treasureButton = $ "#addTreasure"
    @_$monsterButton = $ "#addMonster"
    @_$waveButton = $ "#addWave"

    @_$speed = $ '#simulationSpeed'

    $("#speedSlider").slider {
      range: 'min'
      value: Simulation.speed
      min: 0
      max: Config.maxSimulationSpeed
      step: 1
      slide: (event, ui) =>
        @setSpeed(Config.maxSimulationSpeed-ui.value)
    }
    @setSpeed Simulation.speed

  @setSpeed = (speed) =>
    percentage = (Config.maxSimulationSpeed-speed)/Config.maxSimulationSpeed
    percentage *= 100
    percentage = Math.round percentage
    percentage = Math.max percentage, 1 # no 0%
    $("#speedSlider").slider 'value', Config.maxSimulationSpeed-speed
    @_$speed.html "#{percentage} %"
    Simulation.speed = speed #Config.maxSimulationSpeed-speed



  # switch between gameobject selection with number keys
  @onKeyDown = (event) =>
    return if CodeGUI.isInEditor
    switch event.keyCode
      when 49, 97
        @_$buttons.removeClass "btn-success"
        @_$treasureButton.addClass "btn-success"
      when 50, 98
        @_$buttons.removeClass "btn-success"
        @_$monsterButton.addClass "btn-success"
      when 51, 99
        @_$buttons.removeClass "btn-success"
        @_$waveButton.addClass "btn-success"
      when 27
        @_$buttons.removeClass "btn-success"

  self = this
  @onClick = () ->
    self._$buttons.removeClass "btn-success"
    $(this).addClass "btn-success"


  @creatObjectFromButton = (x, y) ->
    $found = $(".gameObject-controls .btn-success")
    switch $found.attr 'id'
      when 'addWave'
        Grid.addObject (new Wave x, y)
      when 'addTreasure'
        Grid.addObject (new Treasure x, y)
      when 'addMonster'
        Grid.addObject (new Monster x, y)