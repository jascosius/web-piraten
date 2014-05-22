class SocketController <  WebsocketRails::BaseController

  include Preprocessor

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

  def put # event: ship.put
    puts 'Put!'
    WebsocketRails[:operations].trigger(:put)
  end

  def take # event: ship.take
    puts 'Take!'
    WebsocketRails[:operations].trigger(:take)
  end

  def look # event: ship.take
    puts 'Look!'
    WebsocketRails[:operations].trigger(:look)
  end

  def moveShip # event: ship.move
    puts 'Move!'
    WebsocketRails[:operations].trigger(:move)
  end

  def rotateShipRight # event: ship.right
    puts 'Right!'
    WebsocketRails[:operations].trigger(:right)
  end

  def sendLine(line)
    WebsocketRails[:operations].trigger(:line, line)
  end

  def simulateGrid
    receive_code
    grid = message[:grid]
    puts grid
    puts hallo['name']
  end

  def stopSimulation
    puts 'stop'
  end


  def receive_code
    #WebsocketRails[:debug].trigger :console, message[:code]
    puts '================================='
    puts '===== received operation========='
    puts '================================='
    puts ''

    # create temporally file for execution of ruby code
    Dir.mktmpdir("session_") {|dir|
      # use the directory...
      open("#{dir}/code.rb", 'w+') { |file| #TODO: Create file as specific linux user

        code = preprocess_code(message[:code])
        puts "Message: #{code}"

        File.chmod 0777, file
        File.write file, code

        IO.popen "ruby #{File.path(file)}" do |pipe|

          pipe.sync = true
          until pipe.eof?
            line = pipe.readline
            if line.include? 'end'
              #TODO Send error to client
              break
            elsif line.include? 'line'
              sendLine line.split('?')[1].to_i
            elsif line.include? 'turnRight'
               rotateShipRight
            elsif line.include? 'turnLeft'
               rotateShipLeft
            elsif line.include? 'move'
               moveShip
            elsif line.include? 'take'
               take
            elsif line.include? 'look'
              look
            elsif line.include? 'put'
               put
            elsif !line.equal? ''
              #WebsocketRails[:debug].trigger :console, line
            end
            #puts "has \\n? #{line.eql? '\n'} #{line.eql? "\n"} #{line.equal? '\n'} #{line.equal? "\n"}"
            #puts line.dump
          end
        end
      }
      # mktmpdir deletes file automatically
      WebsocketRails[:operations].trigger :done
      puts 'Simulation abgeschlossen'

    }
  end
end