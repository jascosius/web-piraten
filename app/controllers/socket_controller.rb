class SocketController < WebsocketRails::BaseController
  $prefix = 'CkyUHZVL3q_'
  @@timeout = 5 #timeout time for the programm to execute

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

  def simulation_done_error
    puts 'done_error'
    WebsocketRails[:operations].trigger(:done_error)
  end

  def send_line(line)
    WebsocketRails[:operations].trigger(:line, line)
  end

  def simulateGrid
    receive_code
    grid = message[:grid]
    puts grid
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

    code = preprocess_code(message[:code])

    code += "\n#{$prefix}EOF"

    Thread.start do

      Thread.start(Thread.current) do |thread|
        sleep(@@timeout)
        if thread.alive?
          puts 'kill'
          simulation_done_error
          thread.kill
        end
      end

      begin
        #connect to TCPServer to execute the programm
        vm = TCPSocket.open("localhost", 12340)
      rescue
        puts 'Could not connect to TCPSocket. Start ruby app/vm/vm.rb'
      else

        #send programmcode to the server
        vm.puts code
        puts code

        #interact with the tcpserver
        loop do
          line = vm.gets
          if line.include? "#{$prefix}end_error"
            simulation_done_error
            break
          elsif line.include? "#{$prefix}end"
            simulation_done
            break
          elsif line.include? "#{$prefix}line"
            send_line line.split('!')[1].to_i
          elsif line.include? "#{$prefix}turnRight"
            rotate_ship_right
          elsif line.include? "#{$prefix}turnLeft"
            rotate_ship_left
          elsif line.include? "#{$prefix}move"
            move_ship
          elsif line.include? "#{$prefix}take"
            take
          elsif line.include? "#{$prefix}look"
            look
          elsif line.include? "#{$prefix}put"
            put
          elsif !line.equal? ''
            #WebsocketRails[:debug].trigger :console, line
          end
        end
      end

    end
  end
end