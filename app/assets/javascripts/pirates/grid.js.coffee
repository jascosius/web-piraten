class @Grid
  @initialize = (@canvas) ->
    Grid.GridControls.initialize()
    @size = 32 # pixel per cell, overriden by load

    @ctx = canvas.getContext "2d"
    @canvasWidth = @ctx.canvas.width
    @canvasHeight = @ctx.canvas.height

    # load the default grid when the user visits the site for the first time.
    @defaultData = $.parseJSON $('#gridData').html()

    # stores the cell coordinates that are highlighted because of the mouse
    @activeCell = null

    # highlighted cell because of look operation
    @look = null
    @mousePosition = null

    # canvas listeners
    $canvas = $ @canvas
    $canvas.on 'mousedown', this.onClick
    $canvas.on 'mousemove', this.onMouseMove
    $canvas.on 'mouseout', this.onMouseOut
    $canvas.on 'contextmenu', this.onContextmenu
    $canvas.on 'mouseup', this.onMouseUp
    $canvas.on 'selectstart', (event) ->  # no text selection
      event.preventDefault()
      return false

    @ANGLE = (Math.PI/180)
    @blockAnimation = false # toggle animations

    # mouse states
    @mousePressedOnShip = false
    @mousePressed = false
    @_smoothingStep = 0 # help for smooth animation

  @loadDefault = () =>
    @load @defaultData

  ###
    loads a serialized grid object
  ###
  @load = (obj) =>
    @width = obj.width || 22
    @height = obj.height || 10
    @size = obj.size || 32

    s = obj.ship
    @ship = new Ship s.x, s.y, s.rotation


    @objects = []
    for o in obj.objects
      if GameObject.ALL[o.name]? # is the object registered?
        @addObject new GameObject.ALL[o.name](o)
      else
        throw "Invalid GameObject name #{o.name}!"



  # add an object to the grid
  @addObject = (obj) ->
    @objects.push(obj)

  # find and delete object from grid by index in the @objects array
  @deleteObjectWithIndex = (index) ->
    if index != false
      @objects.splice index, 1

  # find and delete an object from the grid
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

  # check if coordinates are within the canvas borders
  @isInCanvas = (coords) =>
    coords.x >= 0 && coords.y >= 0 && coords.x < @canvasWidth && coords.y < @canvasHeight

  # is in grid, pixel based
  @contains = (coords) =>
    @isInCanvas(coords) && coords.x <= @width*@size &&
      coords.y <= @height*@size

  # calculate grid coordinates based on the pixel
  @getGridCoordinates = (coords) ->
    new Coordinate Math.floor(coords.x/@size), Math.floor((coords.y-2)/@size) #because the two pix thick line -2

  # calculate the pixel from the grid coordinates
  @getCanvasCoordinates = (coords) ->
    new Coordinate Math.floor(coords.x*@size), Math.floor(coords.y*@size)

  # get the canvas pixel from the mouse event
  @getMousePos = (mouseEvent) =>
    rect = @canvas.getBoundingClientRect()
    new Coordinate mouseEvent.clientX - rect.left, mouseEvent.clientY - rect.top

  # get a rectangle of a cell, relative to canvas
  @getCellRect = (coords) ->
    x = coords.x*@size
    y = coords.y*@size
    { x1: x, y1: y, x2: @size, y2: @size }

  # click handler
  @onClick = (event) =>
    if !Simulation.isInExecutionMode
      mousPos = @getMousePos event
      pos = @getGridCoordinates mousPos
      objOnPos = @isSomethingOnPosition pos.x, pos.y
      if !(@ship.x == pos.x && @ship.y == pos.y)
        @mousePressed = event.which
      if @ship.x == pos.x && @ship.y == pos.y && event.which == 1
        @mousePressedOnShip = true
      else
        if @contains(mousPos) && objOnPos == false && event.which == 1
          @GridControls.createObjectFromButton pos.x, pos.y
      if event.which == 3 && @ship.x == pos.x && @ship.y == pos.y
        if @ship.rotation <= 0
          @ship.rotation = 3
        else
          @ship.rotation--
      else if event.which == 3 && objOnPos != false
        @deleteObjectWithIndex @objects.indexOf(objOnPos)

  # triggered when the mouse button is released
  @onMouseUp = (event) =>
    @mousePressed = false
    coords = @getGridCoordinates @getMousePos(event)
    x = coords.x
    y = coords.y

    # drop the ship after drag and drop
    if @mousePressedOnShip && Simulation.isInExecutionMode == false
      if !@contains(@getMousePos(event))
        x = coords.x
        y = coords.y
        if x >= @width
          x = @width - 1
        if y >= @height
          y = @height-1
      @ship.x = x
      @ship.y = y

    @mousePressedOnShip = false

  # triggered when the right mouse button is clicked
  @onContextmenu = (event) =>
    event.preventDefault()

  # triggered when the mouse button is
  @onMouseMove = (event) =>

    # track the cell the mouse is hovering above and highlight it
    @activeCell = null
    pos = @getMousePos(event)
    @mousePosition = pos
    if @contains(pos)
      pos = @getGridCoordinates pos
      @activeCell = pos # highlight cell

    # hold right/left mouse button to delete/create objects in the grid
    mousPos = @getMousePos event
    pos = @getGridCoordinates mousPos
    objOnPos = @isSomethingOnPosition pos.x, pos.y
    if !Simulation.isInExecutionMode && objOnPos == false && @mousePressed == 1 && @contains(@getMousePos(event))
      @GridControls.createObjectFromButton pos.x, pos.y
    else if @mousePressed == 3 && objOnPos != false
      @deleteObjectWithIndex @objects.indexOf(objOnPos)


  # triggered when the mouse leaves the area of the canvas
  @onMouseOut = (event) =>
    @mousePressed = false
    @activeCell = null

    # avoid misplacement of the ship, if it is currently being dragged
    if @mousePressedOnShip
      coords = @getGridCoordinates(@getMousePos(event))
      x = coords.x
      y = coords.y
      if x < 0
        x = 0
      else if x >= @width
        x = @width - 1

      if y < 0
        y = 0
      else if y >= @height
        y = @height-1

      @ship.x = x
      @ship.y = y
    @mousePressedOnShip = false

  # check coordinates for gameObject
  @isSomethingOnPosition = (x, y) =>
    for obj in @objects
      if obj.x == x && obj.y == y
        return obj
    false

  # save to a singe javascript object
  @serialize = () =>
    sendObjects = []
    for gameObject in @objects
      sendObjects.push gameObject.serialize()

    sendShip =  @ship.serialize()

    {
      width: @width
      height: @height
      objects: sendObjects
      ship: sendShip
      size: @size
    }

  # once per frame
  @update = () ->
    canceled = !window.dispatchEvent new CustomEvent('beforeGridUpdate', {
      'cancelable': true
    })
    return if canceled
    for gameObject in @objects
      gameObject.update()
    @ship.update()
    window.dispatchEvent new Event('gridUpdated')

  # display grid with canvas
  @draw = () =>
    canceled = !window.dispatchEvent new CustomEvent('beforeGridDraw', {
      'cancelable': true
    })
    return if canceled
    @ctx.save()

    # clear the canvas
    @ctx.clearRect 0, 0, @canvasWidth, @canvasHeight
    window.dispatchEvent new Event('gridDrawClearedCanvas')

    # draw horizontal and vertical lines only the first time
    if not @_cache? or @_cachedData.size != @size or
      @_cachedData.width != @width or
      @_cachedData.height != @height

        @_drawCells Config.lineColor
        @_drawCells Config.lineColor, true
        @_cache = @ctx.getImageData 0, 0, @canvasWidth, @canvasHeight
        @_cachedData = {
          size: @size
          width: @width
          height: @height
        }
    else
      @ctx.putImageData @_cache, 0, 0


    @_drawObjects()
    @_drawShip()

    @_highlightCell @activeCell, Config.cellHighlighting.hovered if @activeCell
    @_highlightCell @look, Config.cellHighlighting.look if @look

    window.dispatchEvent new Event('gridDrawn')
    @ctx.restore()

  # draw one axis of the grid lines.
  @_drawCells = (strokeStyle, vertical) ->
    @ctx.save()
    vertical ||= false
    coords = new Coordinate 0,0 # in pixel

    if vertical
      while(@contains(coords))
        @_drawLine coords.x, 0, coords.x, @height*@size, 1, strokeStyle
        coords.x += @size
    else
      while(@contains(coords))
        @_drawLine 0, coords.y, @width*@size, coords.y, 1, strokeStyle
        coords.y += @size

    @ctx.restore()

  # highlight a cell (by coordinates) with a specific color
  @_highlightCell = (cell, fillStyle) ->
    @ctx.save()
    rect = @getCellRect cell

    @ctx.beginPath()
    @ctx.rect rect.x1, rect.y1, rect.x2, rect.y2
    @ctx.fillStyle = fillStyle
    @ctx.fill()
    @ctx.restore()

  # draw everything from @objects
  @_drawObjects = () ->
    for obj in @objects
      @ctx.save()
      canceled = !window.dispatchEvent new CustomEvent('gridDrawObject', {
        'cancelable': true
        'detail': obj
      })
      @ctx.restore()
      continue if canceled
      @ctx.save()

      # set the size of the drawn image depending on @size
      scaleX = @size/obj.image.width
      scaleY = @size/obj.image.height
      posx = obj.x*@size + Math.floor(obj.image.width*scaleX/2)
      posy = obj.y*@size + Math.floor(obj.image.height*scaleY/2)
      @ctx.translate(posx, posy)

      @ctx.scale scaleX, scaleY
      @ctx.drawImage obj.image, -Math.floor(obj.image.width/2), -Math.floor(obj.image.height/2)
      @ctx.restore()

  # draw the ship with smooth animations and dragging
  @_drawShip = () ->
    @ctx.save()
    event = new CustomEvent('gridDrawShip', {
      'cancelable': true
      'detail': @ship
    })
    canceled = !window.dispatchEvent event
    @ship = event.detail
    @ctx.restore()
    return if canceled

    @ctx.save()
    if @mousePressedOnShip # drag and drop
      # draw centered around the mouse position
      @ctx.translate @mousePosition.x, @mousePosition.y
      @ctx.rotate(@ship.rotation * 90 * @ANGLE)

      @ctx.scale @size/@ship.image.width, @size/@ship.image.height
      @ctx.drawImage(@ship.image, -Math.floor(@ship.image.width/2), -Math.floor(@ship.image.height/2))
    else
      # resize depending on @size
      widthFactor = @size/@ship.image.width
      heightFactor = @size/@ship.image.height
      newWidth = widthFactor*@ship.image.width
      newHeight = heightFactor*@ship.image.height

      cellCenter = {
        x: @ship.x*@size + (Math.floor(newWidth/2))
        y: @ship.y*@size + Math.floor(newHeight/2)
      }

      # check if a new operation should be animated
      if !@_lastPacket?
        @_lastPacket = SocketHandler.packetCounter

      if @_lastPacket != SocketHandler.packetCounter # old packet we are working on
        @_lastPacket = SocketHandler.packetCounter
        @_smoothingStep = 0

      # calculate the point of the drawing with the animation algorithm
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

  # smooth movements of the ship
  @_smoothShipMovement = (cellCenter) ->
    if !@ship.isMoving or @blockAnimation
      @_smoothingStep = 0
      @ship.isMoving = false
      return cellCenter

    axis = -1 #vertical
    direction = -1 # increase ship coordinate

    if @ship.rotation % 2 is 0 # 0 or 2
      axis = 1 #horizontal
    if @ship.rotation in [0,1] # 1 or 3
      direction = 1 # decrease ship coordinate

    speed = Simulation.speed
    speed = Math.max speed, 0.00001 # can't be 0
    pxPerFrame = @size/speed # how many pixel has the ship to move each frame

    # rotation mechanism is different depending on the axis
    shipAxis = 'x' #horizontal
    if axis < 0
      shipAxis = 'y' # vertical

    cellCenter[shipAxis] = (@ship[shipAxis] - direction)*@size+(@size*0.5)
    cellCenter[shipAxis] += pxPerFrame*@_smoothingStep*direction

    # keep track of how many animation steps have been executed already, usable for easing
    @_smoothingStep++
    # check if smoothing is/should be done
    if @_smoothingStep >= speed-1
      @ship.isMoving = false

    return cellCenter

  # smooth rotation movements of the ship
  @_smoothShipRotation = () ->
    if !@_smoothingRotationStep?
      @_smoothingRotationStep = 0

    if @blockAnimation
      @ship.isRotating = false
      return

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

  # draw a line on the grid between [x1,y] and [x1,y2] with specific line width and style
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
  @initialize = () ->
    $(document).on "keydown", @onKeyDown

    #preload queries for optimization
    @_$buttons = $ ".gameObject-controls button"
    @_$buttons.click @onClick
    @_$treasureButton = $ "#addTreasure"
    @_$monsterButton = $ "#addMonster"
    @_$waveButton = $ "#addWave"


  # switch between gameObject selection with number keys
  @onKeyDown = (event) =>
    return if CodeGUI.isInEditor
    switch event.keyCode
      when 49, 97 # 1 key
        @_$buttons.removeClass "btn-success"
        @_$treasureButton.addClass "btn-success"
      when 50, 98 # 2 key
        @_$buttons.removeClass "btn-success"
        @_$monsterButton.addClass "btn-success"
      when 51, 99 # 3 key
        @_$buttons.removeClass "btn-success"
        @_$waveButton.addClass "btn-success"
      when 27 # escape
        @_$buttons.removeClass "btn-success"

  self = this # save context because it changes in events
  # click event handler for the gameObject buttons
  @onClick = () ->
    self._$buttons.removeClass "btn-success"
    $(this).addClass "btn-success"


  # get the gameObject from the buttons, by reading data-object attribute from HTML
  @createObjectFromButton = (x, y) ->
    $found = $(".gameObject-controls .btn-success")
    typeName = $found.data 'object' # read the data-object="x" attribute
    console.log "class", typeName
    clazz = GameObject.getGameObjectClass typeName
    unless clazz?
      throw "unknown object class #{obj}"

    Grid.addObject (new clazz x, y)
