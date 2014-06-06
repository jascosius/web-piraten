@Config =
  simulationSpeed: 60 # time in ticks between each action
  simulationDelay: 60 # time in ticks before the first action starts
  buoyImage: "./assets/buoy.png"
  shipImage: "./assets/ship.png"
  waveImage: "./assets/wave.png"
  treasureImage: "./assets/coins.png"
  monsterImage: "./assets/monster.png"
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
