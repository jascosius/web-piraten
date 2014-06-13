@Config =
  simulationSpeed: 0 # time in ticks between each action
  maxSimulationSpeed: 120
  buoyImage: "./assets/frank/buoy.gif"
  shipImage: "./assets/frank/ship.gif"
  waveImage: "./assets/frank/wave.gif"
  treasureImage: "./assets/frank/treasure.gif"
  monsterImage: "./assets/frank/monster.gif"
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
