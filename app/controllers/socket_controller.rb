class SocketController < WebsocketRails::BaseController

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
  def rotate_ship_left # event: ship.left
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

  def move_ship # event: ship.move
    puts 'Move!'

    WebsocketRails[:operations].trigger(:move)
  end

  def rotate_ship_right # event: ship.right
    puts 'Right!'
    WebsocketRails[:operations].trigger(:right)
  end

  def simulation_done
    puts 'done'
    WebsocketRails[:operations].trigger(:done)
  end

  def send_line(line)
    WebsocketRails[:operations].trigger(:line, line)
  end

  def read_JSON
    grid = message[:grid]
    puts grid
    x = grid['width']
    y = grid['height']
    @grid_size = [x,y]
    @objects = grid['objects'].to_a
    @ship = grid['ship']
    #puts obja.to_s
    #@objects.each{ |obj|
    #  if  obj['name'] == 'Monster'
    #    puts obj
    #  end
    #}
  end

  def stopSimulation
    puts 'stop'
  end




  def receive_code
    read_JSON
    prefix = 'CkyUHZVL3q_'
    #WebsocketRails[:debug].trigger :console, message[:code]
    puts '================================='
    puts '===== received operation========='
    puts '================================='
    puts ''

    code = preprocess_code(message[:code])

    code += "\n#{prefix}EOF"

    begin
      #connect to TCPServer to execute the programm
      vm = TCPSocket.open("localhost", 12340)
    rescue
      puts 'Could not connect to TCPSocket. Start ruby app/vm/vm.rb'
    else

      #send programmcode to the server
      vm.puts code

      #interact with the tcpserver
      loop do
        line = vm.gets
        if line.include? "#{prefix}_end"
          simulation_done
          break
        elsif line.include? 'line'
          send_line line.split('?')[1].to_i
        elsif line.include? 'turnRight'
          rotate_ship_right
        elsif line.include? 'turnLeft'
          rotate_ship_left
        elsif line.include? 'move'
          move_ship
        elsif line.include? 'take'
          take
        elsif line.include? 'look'
          look
        elsif line.include? 'put'
          put
        elsif !line.equal? ''
          #WebsocketRails[:debug].trigger :console, line
        end
      end

    end
  end
end