# -*- encoding : utf-8 -*-

class SocketController < WebsocketRails::BaseController
  $prefix = 'CkyUHZVL3q_'
  @@timeout = 5 #timeout time for the programm to execute
  @@port = 12340 #port to connect to the vm
  @@host = 'localhost' #host to connect to the vm

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
  def rotate_ship(direction) # event: ship.left
    if !@is_simulation_done
      case direction
        when :left
          puts 'Left!'
          @ship['rotation'] = (@ship['rotation'] - 1) % 4
        when :right
          puts 'Right!'
          @ship['rotation'] = (@ship['rotation'] + 1) % 4
        when :back
          puts 'Back!'
          @ship['rotation'] = (@ship['rotation'] + 2) % 4
      end
      WebsocketRails[:operations].trigger(:turn, @ship['rotation'])
    end
  end

  def put # event: ship.put
    if !@is_simulation_done
      puts 'Put!'
      WebsocketRails[:operations].trigger(:put)
    end
  end

  def take # event: ship.take
    if !@is_simulation_done
      puts 'Take!'
      WebsocketRails[:operations].trigger(:take)
    end
  end

  def look(direction) # event: ship.take
    if !@is_simulation_done
      puts 'Look!'
      coord = [@ship['x'], @ship['y']]
      next_coord = get_next_position
      rotate = @ship['rotation']
      puts rotate
      case direction
        when :front
          coord = next_coord
        when :back
          coord[0] -= next_coord[0] - coord[0]
          coord[1] -= next_coord[1] - coord[1]
        when :left
          case rotate
            when 3
              coord[0] += 1
            when 1
              coord[0] -= 1
            when 0
              coord[1] -= 1
            when 2
              coord[1] += 1
          end
        when :right
          case rotate
            when 3
              coord[0] -= 1
            when 1
              coord[0] += 1
            when 0
              coord[1] += 1
            when 2
              coord[1] -= 1
          end
      end
      if coords_in_grid(coord) == false
        look_obj = 'Border'
      else
        @objects.each { |obj|
          x = obj['x']
          y = obj['y']
          if  [x, y] == coord
            look_obj = obj['name'].to_s
            puts obj['name']
          end
        }
      end
      WebsocketRails[:operations].trigger(:look, coord)
    else
      look_obj='stop'
    end
    look_obj
  end

  def move_ship # event: ship.move
    if !@is_simulation_done
      puts 'Move!'
      coord = get_next_position
      if coords_in_grid(coord)
        @ship['x'] = coord[0]
        @ship['y'] = coord[1]
        WebsocketRails[:operations].trigger(:move, coord)
      else
        @is_simulation_done = true
        WebsocketRails[:operations].trigger(:done_error, 'Spielfeld verlassen')
      end
    end

  end

  def coords_in_grid(coord)
    if coord[0] >= 0 && coord[0] < @grid_size[0] && coord[1] >= 0 && coord[1]< @grid_size[1]
      true
    else
      false
    end
  end

  def get_next_position
    x = @ship['x']
    y = @ship['y']
    case @ship['rotation']
      when 0
        x += 1
      when 1
        y += 1
      when 2
        x -= 1
      when 3
        y -= 1
    end
    [x, y]
  end

  def simulation_done
    if !@is_simulation_done
      puts 'done'
      WebsocketRails[:operations].trigger(:done)
      @is_simulation_done = true
    end

  end

  def simulation_done_error(msg)
    if !@is_simulation_done
      puts 'done_error'
      WebsocketRails[:operations].trigger(:done_error, "Ausfuehrung beendet: #{msg}")
      @is_simulation_done = true
    end

  end

  def send_line(line)
    if !@is_simulation_done
      WebsocketRails[:operations].trigger(:line, line)
    end

  end

  def puts_user_output(line)
    if !@is_simulation_done
      puts line
      line = CGI::escapeHTML(line)
      new_line = ''
      line.each_char do |c|
        if c == ' '
          new_line += '&nbsp;'
        elsif c == "\t"
          new_line += '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'
        else
          new_line += c
        end
      end
      WebsocketRails[:operations].trigger(:output, new_line)
    end
  end

  def puts_user_output_error(line)
    if !@is_simulation_done
      line = CGI::escapeHTML(line)
      new_line = ''
      line.each_char do |c|
        if c == ' '
          new_line += '&nbsp;'
        elsif c == "\t"
          new_line += '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'
        else
          new_line += c
        end
      end
      WebsocketRails[:operations].trigger(:output_error, new_line)
    end
  end

  def read_JSON
    grid = message[:grid]
    puts grid
    x = grid['width']
    y = grid['height']
    @grid_size = [x, y]
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
    @is_simulation_done = true
    puts 'stop'
  end

  def receive_code
    @is_simulation_done = false
    read_JSON
    #WebsocketRails[:debug].trigger :console, message[:code]
    puts '================================='
    puts '===== received operation========='
    puts '================================='
    puts ''

    code = preprocess_code(message[:code])

    #add EOF to show Wrapper the end of the code
    code += "\n#{$prefix}EOF\n"

    Thread.start do

      #Thread to stop the execution after timeout time
      Thread.start(Thread.current) do |thread|
        sleep(@@timeout)
        if thread.alive?
          puts 'kill'
          simulation_done_error 'Ausführungszeit wurde überschritten.'
          thread.kill
        end
      end

      begin
        #connect to TCPServer to execute the programm
        vm = TCPSocket.open(@@host, @@port)
      rescue
        puts 'Could not connect to TCPSocket. Start ruby app/vm/vm.rb'
        simulation_done_error 'Ein interner Fehler ist aufgetreten.'
      else

        #send commands to the server
        vm.puts preprocess_filename
        vm.puts preprocess_compile
        vm.puts preprocess_execute
        vm.puts preprocess_compile_error
        vm.puts preprocess_execute_error

        #send programmcode to the server
        vm.puts code
        puts code

        #interact with the tcpserver
        loop do
          line = vm.gets
          if line.include? "#{$prefix}end_error"
            line.slice!("#{$prefix}end_error")
            line.gsub!($prefix,'')
            simulation_done_error line
            break
          elsif line.include? "#{$prefix}end"
            simulation_done
            break
          elsif line.include? "#{$prefix}stderr_compile"
            line.slice!("#{$prefix}stderr_compile")
            line = postprocess_error_compile(line, code)
            line.gsub!($prefix,'')
            puts line
            puts_user_output_error line
          elsif line.include? "#{$prefix}stderr"
            line.slice!("#{$prefix}stderr")
            line = postprocess_error(line, code)
            line.gsub!($prefix,'')
            puts line
            puts_user_output_error line
          elsif line.include? "#{$prefix}line"
            send_line line.split('!')[1].to_i
          elsif line.include? "#{$prefix}turn_right"
            rotate_ship(:right)
          elsif line.include? "#{$prefix}turn_left"
            rotate_ship(:left)
          elsif line.include? "#{$prefix}turn_back"
            rotate_ship(:back)
          elsif line.include? "#{$prefix}move"
            move_ship
          elsif line.include? "#{$prefix}take"
            take
          elsif line.include? "#{$prefix}?_look_right"
            vm.puts "#{$prefix}!_#{look(:right)}"
          elsif line.include? "#{$prefix}?_look_left"
            vm.puts "#{$prefix}!_#{look(:left)}"
          elsif line.include? "#{$prefix}?_look_back"
            vm.puts "#{$prefix}!_#{look(:back)}"
          elsif line.include? "#{$prefix}?_look_front"
            vm.puts "#{$prefix}!_#{look(:front)}"
          elsif line.include? "#{$prefix}?_look_here"
            test = "#{$prefix}!_#{look(:here)}"
            puts test
            vm.puts test
          elsif line.include? "#{$prefix}put"
            put
          elsif !line.chomp.empty?
            line.gsub!($prefix,'')
            puts_user_output line
          end
        end
      end
    end
  end
end
