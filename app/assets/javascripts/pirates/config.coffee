@Config =
  maxSimulationSpeed: 120 # time in ticks between each action
  buoyImage: "./assets/frank/buoy.gif"
  shipImage: "./assets/frank/ship.gif"
  waveImage: "./assets/frank/wave.gif"
  treasureImage: "./assets/frank/treasure.gif"
  monsterImage: "./assets/frank/monster.gif"
  lineColor: '#999'
  cellHightlighting:
    hovered: 'rgba(0,0,0,0.1)'
    look: 'rgba(255,0,0,0.2)'

  getImagesToPreload: () ->
    [
      @buoyImage
      @shipImage
      @monsterImage
      @waveImage
      @treasureImage
    ]
  getWatchListRemoveButtonHTML: (word) ->
    "<li>
      <span class='glyphicon glyphicon-remove watchlist-remove' aria-hidden='true'></span>
      #{word}
    </li>"
