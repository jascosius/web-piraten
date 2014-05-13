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

  def sendLine(line)
    WebsocketRails[:operations].trigger(:line, line)
  end

  def receive_code
    WebsocketRails[:debug].trigger :console, message[:code]

    # create temporally file for execution of ruby code
    Dir.mktmpdir("session_") {|dir|
      # use the directory...
      open("#{dir}/code.rb", 'w+') { |file| #TODO: Create file as specific linux user

        code = preprocessCode
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
            end
            puts line
          end
        end
      }
      # mktmpdir deletes file automatically
    }
  end

  private

  def preprocessCode
    i=0
    code = ''
    message[:code].each_line do |s|
      code = code + "line(#{i+=1})\n" + s
    end

    code = injectShipLogic(code)
  end


  def injectShipLogic(code)
    'def move
       puts "move"
     end
     def turnRight
       puts "turnRight"
     end
     def turnLeft
       puts "turnLeft"
     end
     def line(i)
       puts "line?#{i}"
     end
    ' + code
  end
end