class @Utils

  @requestAnimFrame: (mainLoop) ->
    window.requestAnimationFrame(mainLoop)       ||
    window.webkitRequestAnimationFrame(mainLoop) ||
    window.mozRequestAnimationFrame(mainLoop)    ||
    window.oRequestAnimationFrame(mainLoop)      ||
    window.msRequestAnimationFrame(mainLoop)     ||
    (mainLoop) ->
      window.setTimeout mainLoop, 1000 / 60

  @createFileUpload = (parameters) ->
    defaults = {
      fileExtension: false
      onSuccess: () -> # void
      onInvalidFile: () -> # void
    }
    options = $.extend {}, defaults, parameters
    # create a pseudo file uploader
    $uploader = $ '<input type="file" id="fileUploader" style="display:none"></div>'
    $uploader = $uploader.appendTo 'body'
    suffix = options.fileExtension
    successCallback = options.onSuccess
    failCallback = options.onInvalidFile

    # get the selected file
    $uploader.change (event) => # file picked
      file = event.target.files[0] # not a multifile upload
      name = file.name
      # file ends with correct suffix?
      if suffix and name.indexOf(suffix, name.length - suffix.length) < 0
        $uploader.remove()
        failCallback()
      else
        reader = new FileReader()
        # callback once the file is loaded
        reader.onload = (file) -> # after the reader loaded the file
          content = @result
          successCallback content
          $uploader.remove()

        # initiate reading, will trigger reader.onload
        reader.readAsText file
    # trigger the file picker
    $uploader.click()

class @Coordinate
  constructor: (@x,@y) ->
    # everything is done thanks to @

class @Operation
  constructor: (@event, @data) ->
