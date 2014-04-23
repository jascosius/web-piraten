class SocketController <  WebsocketRails::BaseController
  def initialize_session
    puts 'new_event was called'
  end

  def client_connected
    puts 'client connected!'
  end

  def client_disconnected
    puts 'client disconnected!'
  end

  # test events for the remote control buttons
  def rotateShipLeft # event: ship.left
    puts 'Left!'
    WebsocketRails[:operations].trigger(:left)
  end

  def moveShip # event: ship.move
    puts 'Move!'
    WebsocketRails[:operations].trigger(:move)
  end

  def rotateShipRight # event: ship.right
    puts 'Right!'
    WebsocketRails[:operations].trigger(:right)
  end

  def receive_code

    code = injectShipLogic message[:code]
    puts code
    #instance_eval(code)
    WebsocketRails[:debug].trigger :console, message[:code]
    #WebsocketRails[:debug].trigger(:console, Opal.compile(code))

  end

  def test_event
    puts '=============================='
    puts 'TEST EVENT!'
    puts '=============================='
    puts message #TODO: Daten vom Client?
    msg = {:message => 'Message from test_event!'}
    send_message :test, msg
  end

  private

  def injectShipLogic(code)
    'def move
      `window.ship.move()`
    end
    def turnLeft
      `window.ship.rotateLeft()`
    end
    def turnRight
      `window.ship.rotateRight()`
    end
    ' + code
  end
end