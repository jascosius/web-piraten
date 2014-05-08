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

    #code = injectShipLogic message[:code]
    #puts code
    #instance_eval(code)
    WebsocketRails[:debug].trigger :console, message[:code]
    #WebsocketRails[:debug].trigger(:console, Opal.compile(code))


    # erstelle temporäre Datei



    # datei mit zusätzlichem Code anreichern
    # führe datei mit ruby interpreter aus

    # lösche datei
    Dir.mktmpdir("session_") {|dir|
      # use the directory...
      open("#{dir}/code.rb", 'w+') { |file| #TODO: Create file as specific linux user
        puts "Message: #{message[:code]}"

        #sleep(10)
        File.chmod(0777, file)
        #sleep(1)
        File.write(file, message[:code])

        IO.popen "ruby #{File.path(file)}" do |pipe|

          # until pipe.eof?
          #   buffer = pipe.gets
          #   pipe.puts buffer;
          #   puts "2: #{buffer}"
          #
          #   pipe.flush
          # end
          pipe.sync = true
          until pipe.eof?
            line = pipe.readline
            if line.include? 'end' then
              puts 'Ausführung abgebrochen!'
              break
            elsif line.include? 'turnRight' then
               rotateShipRight()
            elsif line.include? 'move' then
               moveShip()
             end

            puts line
          end
        end
        puts 'simulation stopped!'
      }
      # mktmpdir deletes file automatically
    }
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