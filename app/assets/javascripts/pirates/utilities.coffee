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

  @createSaveDialog = ($selection, id, extension, contentGenerator) ->
    defaultFileName = "webpiraten-#{id}"
    formId = "saveFileName-#{id}"
    $selection.popover {
      placement: 'bottom'
      trigger: 'click'
      title: 'Dateiname wählen'
      html: true
      content: "
               <form action='#' id='#{formId}' class='saveDialog form-inline'>
                 <input type='text' value='#{defaultFileName}' class='filename form-control' required /> .#{extension}<br />
                 <a download='#{defaultFileName}' class='btn btn-success btn-block'>Speichern</a>
               </form>"
      container: '.modal-body'
    }
    # wait for the popover to exist
    $selection.on 'shown.bs.popover', () ->
      $form = $ "\##{formId}"
      console.log $form
      $submitButton = $form.find 'a.btn'
      console.log $submitButton
      # hitting enter in the input field
      $form.off 'submit'
      $form.submit (event) =>
        event.preventDefault()
        event.stopPropagation()
        return false

      # start download
      $submitButton.off 'click'
      $submitButton.click () =>
        console.log 'clicked save', id
        # create a data URL that will trigger a download
        data = 'data:text/plain;charset=UTF-8,'
        data += encodeURIComponent contentGenerator()
        btn = $submitButton.get 0
        btn.href = data
        console.log btn
        # chrome will set the file name
        fileName = $form.find('.filename').val()
        fileName = defaultFileName if !fileName? or fileName.length < 1
        fileName += ".#{extension}"
        $(btn).attr 'download', fileName
        $selection.popover 'hide'


class @Coordinate
  constructor: (@x,@y) ->
    # everything is done thanks to @

class @Operation
  constructor: (@event, @data) ->
