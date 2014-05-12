class @Grid
  constructor: (@canvas, @size) ->
    @ctx = canvas.getContext "2d"
    @canvasWidth = @ctx.canvas.width
    @canvasHeight = @ctx.canvas.height
    @offsetX = @canvasWidth % @size
    @offsetY = @canvasHeight % @size
    @activeCell = null
    @gridWidth = Math.floor (@canvasWidth/@size)
    @gridHeight = Math.floor (@canvasHeight/@size)
    @objects = []
    @objectGrid = new Array(@gridWidth)

    @objectGrid[i] = new Array(@gridHeight) for i in [0.. (@gridWidth-1)]

    @operationQueue = [] # stores every operation the server sends that is not done yet
    @history = [] # list of every operation send by the server

    @canvas.addEventListener "mousedown", this.onClick, false;
    @canvas.addEventListener "mousemove", this.onMouseMove, false;
    @canvas.addEventListener "mouseout", this.onMouseMove, false;

    @ANGLE = 90*(Math.PI/180)

  update: (deltaTime) ->
    for gameObject in @objects
      gameObject.update(deltaTime)

  draw: =>
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
    for i in [0.. (@objects.length-1)]
      obj = @objects[i]

      objCoords = new Coordinate obj.x, obj.y

      @ctx.save()
      posx = objCoords.x*@size + Math.floor(obj.image.width/2)
      posy = objCoords.y*@size + Math.floor(obj.image.height/2)
      @ctx.translate(posx, posy)

      @ctx.rotate(obj.rotation * @ANGLE)

      if obj.rotation == 2
          @ctx.scale 1, -1 # flip
      @ctx.drawImage(obj.image, -Math.floor(obj.image.width/2), -Math.floor(obj.image.height/2))
      @ctx.restore()

    if @activeCell
      backup = @ctx.fillStyle
      rect = @getCellRect @activeCell

      @ctx.beginPath()
      @ctx.rect rect.x1, rect.y1, rect.x2, rect.y2
      @ctx.fillStyle = 'rgba(0,0,0,0.1)'
      @ctx.fill()
      @ctx.fillStyle = backup

  # end draw

  addObject: (obj) ->
    @objects.push(obj)

  isInCanvas: (coords) ->
    coords.x >= 0 && coords.y >= 0 && coords.x < @canvasWidth && coords.y < @canvasHeight

  contains: (coords) ->
    @isInCanvas(coords) && coords.x < Math.floor(@canvasWidth/@size)*@size &&
      coords.y < Math.floor(@canvasHeight/@size)*@size

  getGridCoordinates: (coords) ->
    new Coordinate Math.floor(coords.x/@size), Math.floor(coords.y/@size)

  getCanvasCoordinates: (coords) ->
    new Coordinate Math.floor(coords.x*@size), Math.floor(coords.y*@size)

  getMousePos: (mouseEvent) =>
    rect = @canvas.getBoundingClientRect()
    new Coordinate mouseEvent.clientX - rect.left, mouseEvent.clientY - rect.top

  # relative to canvas
  getCellRect: (coords) ->
    x = coords.x*@size
    y = coords.y*@size
    { x1: x, y1: y, x2: @size, y2: @size }

  onClick: (event) =>
    console.log(@getMousePos(event))

  onMouseMove: (event) =>
    @activeCell = null
    pos = @getMousePos(event)
    if @contains(pos)
      pos = @getGridCoordinates pos
      @activeCell = pos

  onMouseOut: (event) =>
    @activeCell = null



  drawLine: (x1, y1, x2, y2, width, strokeStyle) ->
    newX1 = Math.min(x1, x2)
    newY1 = Math.min(y1, y2)
    newX2 = Math.max(x1, x2)
    newY2 = Math.max(y1, y2)

    @ctx.save()
    @ctx.beginPath()

    # width = 1 fix, should not be 2 pixel thick
    if width <= 1
      newX1 += 0.5
      newY1 += 0.5
      newX2 += 0.5
      newY2 += 0.5

    @ctx.moveTo(newX1,newY1)
    @ctx.lineTo(newX2,newY2)
    @ctx.closePath()
    @ctx.lineWidth = width
    @ctx.strokeStyle = strokeStyle
    @ctx.stroke()

    @ctx.restore()

