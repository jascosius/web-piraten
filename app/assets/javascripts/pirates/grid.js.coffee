class @Grid
  @initialize = (@canvas, @size) ->
    Grid.GridControls._initialize()
    @ctx = canvas.getContext "2d"
    @canvasWidth = @ctx.canvas.width
    @canvasHeight = @ctx.canvas.height
    @offsetX = @canvasWidth % @size
    @offsetY = @canvasHeight % @size
    @gridWidth = Math.floor (@canvasWidth/@size)
    @gridHeight = Math.floor (@canvasHeight/@size)

    @activeCell = null
    @look = null
    @mousePosition = null

    @ship = null
    @objects = []

    $canvas = $(@canvas)
    $canvas.on 'mousedown', this.onClick
    $canvas.on 'mousemove', this.onMouseMove
    $canvas.on 'mouseout', this.onMouseOut
    $canvas.on 'contextmenu', this.onContextmenu
    $canvas.on 'mouseup', this.onMouseUp
    $canvas.on 'selectstart', () -> return false

    @ANGLE = 90*(Math.PI/180)

    @mousePressedOnShip = false

  @update = () ->
    for gameObject in @objects
      gameObject.update(Simulation.deltaTime)

  @draw = () =>
    count = 0
    strokeStyle = "#999"

    coords = new Coordinate 0,0

    # draw horizontal lines
    while(@isInCanvas(coords))
      @drawLine 0, coords.y, @canvasWidth - @offsetX, coords.y, 1, strokeStyle
      coords.y += @size
      count++

    count = 0
    coords = new Coordinate 0, 0

    # draw vertical
    while(@isInCanvas(coords))
      this.drawLine coords.x, 0, coords.x, @canvasHeight - @offsetY, 1, strokeStyle
      coords.x += @size
      count++

    # draw objects
    for i in [0.. (@objects.length)]
      if i != @objects.length
        obj = @objects[i]
        @ctx.save()
        posx = obj.x*@size + Math.floor(obj.image.width/2)
        posy = obj.y*@size + Math.floor(obj.image.height/2)
        @ctx.translate(posx, posy)

        @ctx.rotate(obj.rotation * @ANGLE)

        if obj.rotation == 2
            @ctx.scale 1, -1 # flip
        @ctx.drawImage(obj.image, -Math.floor(obj.image.width/2), -Math.floor(obj.image.height/2))
        @ctx.restore()


    #draw activeCell
    if @activeCell
      @ctx.save()
      rect = @getCellRect @activeCell

      @ctx.beginPath()
      @ctx.rect rect.x1, rect.y1, rect.x2, rect.y2
      @ctx.fillStyle = 'rgba(0,0,0,0.1)'
      @ctx.fill()

      @ctx.restore()

    #draw look
    if @look
      @ctx.save()
      rect = @getCellRect @look

      @ctx.beginPath()
      @ctx.rect rect.x1, rect.y1, rect.x2, rect.y2
      @ctx.fillStyle = 'rgba(255,0,0,0.2)'
      @ctx.fill()

      @ctx.restore()

    #draw ship
    @ctx.save()
    if @mousePressedOnShip
      obj = @ship

      posx = @mousePosition.x
      posy = @mousePosition.y
      @ctx.translate(posx, posy)

      @ctx.drawImage(obj.image, -Math.floor(obj.image.width/2), -Math.floor(obj.image.height/2))
    else
      posx = @ship.x*@size + Math.floor(@ship.image.width/2)
      posy = @ship.y*@size + Math.floor(@ship.image.height/2)
      @ctx.translate(posx, posy)

      @ctx.rotate(@ship.rotation * @ANGLE)

      if @ship.rotation == 2
        @ctx.scale 1, -1 # flip
      @ctx.drawImage(@ship.image, -Math.floor(@ship.image.width/2), -Math.floor(@ship.image.height/2))
    @ctx.restore()


  # end draw

  @addObject = (obj) ->
    @objects.push(obj)

  @deleteObject = (obj) ->
    if obj != false
      newObjects = []
      for gameObject in @objects
        if gameObject != obj
          newObjects.push(gameObject)
      @objects = newObjects

  @isInCanvas = (coords) ->
    coords.x >= 0 && coords.y >= 0 && coords.x < @canvasWidth && coords.y < @canvasHeight

  @contains = (coords) ->
    @isInCanvas(coords) && coords.x < Math.floor(@canvasWidth/@size)*@size &&
      coords.y < Math.floor(@canvasHeight/@size)*@size

  @getGridCoordinates = (coords) ->
    new Coordinate Math.floor(coords.x/@size), Math.floor(coords.y/@size)

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
    mousPos = @getMousePos(event)
    pos = @getGridCoordinates mousPos
    if @ship.x == pos.x && @ship.y == pos.y && event.which == 1
      @mousePressedOnShip = true
    else
      if @contains(mousPos) && @isSomethingOnPosition(pos.x, pos.y) == false && !Simulation.isSimulating && event.which == 1
        @GridControls.creatObjectFromButton(pos.x, pos.y)
    if event.which == 3 && @isSomethingOnPosition(pos.x,pos.y).name != "PirateShip"
      @deleteObject (@isSomethingOnPosition(pos.x,pos.y))

  @onMouseUp = (event) =>
    coords = @getGridCoordinates @getMousePos(event)
    x = coords.x
    y = coords.y
    if @mousePressedOnShip
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

  @onMouseOut = (event) =>
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

  @drawLine = (x1, y1, x2, y2, width, strokeStyle) ->
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


