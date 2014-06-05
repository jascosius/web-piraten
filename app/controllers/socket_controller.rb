# -*- encoding : utf-8 -*-

class SocketController < WebsocketRails::BaseController
  $prefix = 'CkyUHZVL3q_'
  $debugprefix = 'BvDpuDu4Be_'
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
  def turn!(packet, direction) # event: ship.left
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
      packet[:operations] ||= []
      packet[:operations] << {name: 'turn', return: @ship['rotation']}
      #WebsocketRails[:operations].trigger(:turn, @ship['rotation'])
    end
  end

  def put!(packet, elem) # event: ship.put
    if !@is_simulation_done
      case elem
        when :buoy
          name = "Buoy"
        when :treasure
          name = "Treasure"
      end
      if !@objects.any? { |obj| obj['x'] == @ship['x'] && obj['y'] == @ship['y'] }
        @objects.push({"name" => name, "x" => @ship['x'], "y" => @ship['y']})
        puts 'Put!'
        packet[:operations] ||= []
        packet[:operations] << {name: 'put', return: name}
        #WebsocketRails[:operations].trigger(:put, name)
      else
        packet[:operations] << {name: 'put', return: false}
        #WebsocketRails[:operations].trigger(:put, false)
      end
    end
  end

  def take!(packet) # event: ship.take
    if !@is_simulation_done
      coord = [@ship['x'], @ship['y']]
      puts @objects.to_s
      @objects.each_with_index { |obj, index|
        x = obj['x']
        y = obj['y']
        if  [x, y] == coord
          if obj['name'] == 'Treasure'
            @objects.delete_at index
            puts index
            #WebsocketRails[:operations].trigger(:take, index)
            puts 'Take!'
            packet[:operations] ||= []
            packet[:operations] << {name: 'take', return: index}
          end
        end
      }


    end
  end

  def look!(packet, direction) # event: ship.take
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
      packet[:operations] ||= []
      packet[:operations] << {name: 'look', return: coord}
      #WebsocketRails[:operations].trigger(:look, coord)
    else
      look_obj='stop'
    end
    look_obj
  end

  def move!(packet) # event: ship.move
    if !@is_simulation_done
      puts 'Move!'
      coord = get_next_position
      if coords_in_grid(coord)
        @ship['x'] = coord[0]
        @ship['y'] = coord[1]
        #WebsocketRails[:operations].trigger(:move, coord)
        packet[:operations] ||= []
        packet[:operations] << {name: 'move', return: coord}
      else
        @is_simulation_done = true
        packet[:operations] ||= []
        packet[:operations] << {name: 'move'}
        packet[:messages] ||= []
        packet[:messages] << {type: 'warning', message: 'Spielfeld verlassen'}
        #WebsocketRails[:operations].trigger(:done_error, 'Spielfeld verlassen')
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

  def done!(packet)
    if !@is_simulation_done
      packet[:operations] ||= []
      packet[:operations] << {name: 'done'}
      @is_simulation_done = true
    end

  end

  def print!(packet, line, type)
    if !@is_simulation_done
      remove_prefix! line
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
      packet[:messages] ||= []
      packet[:messages] << {type: type, message: line}
      #WebsocketRails[:operations].trigger(:output, new_line)
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

  def remove_prefix!(string)
    string.gsub!($prefix, '')
    string.gsub!($debugprefix, '')
  end

  def send_packet(packet)
    if packet != []
      puts packet
      WebsocketRails[:simulation_step].trigger(:packet, packet)
      packet.clear
    end
  end

  def receive_code

    packet = Hash.new()
    id = rand(10000000000)

    @is_simulation_done = false
    read_JSON
    #WebsocketRails[:debug].trigger :console, message[:code]
    puts '================================='
    puts '===== received operation========='
    puts '================================='
    puts ''

    tracing_vars = message[:vars]
    language = 'Ruby'
    code = preprocess_code(message[:code], language, tracing_vars)

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
          if  line.include? "#{$debugprefix}"
            index_begin = $debugprefix.length
            index_end = line.index('!')
            index = line[index_begin...index_end].to_i
            var_name = tracing_vars[index]
            var_value = line[index_end+1..-1]
            remove_prefix! var_name
            remove_prefix! var_value
            print!(packet, var_name + " ist " + var_value, :log) # TODO implement functional and beautiful method!
          elsif line.include? "#{$prefix}end_error"
            line.slice!("#{$prefix}end_error")
            print!(packet, line, :error)
            break
          elsif line.include? "#{$prefix}end"
            done!(packet)
            break
          elsif line.include? "#{$prefix}stderr_compile"
            line.slice!("#{$prefix}stderr_compile")
            line = postprocess_error_compile(line, code)
            puts line
            print!(packet, line, :error)
          elsif line.include? "#{$prefix}stderr"
            line.slice!("#{$prefix}stderr")
            line = postprocess_error(line, code)
            puts line
            print!(packet, line, :error)
          elsif line.include? "#{$prefix}line"
            send_packet(packet)
            packet[:id] = id
            packet[:line] = line.split('!')[1].to_i #send_line line.split('!')[1].to_i
          elsif line.include? "#{$prefix}turn_right"
            turn!(packet, :right) #rotate_ship(:right)
          elsif line.include? "#{$prefix}turn_left"
            turn!(packet, :left)
          elsif line.include? "#{$prefix}turn_back"
            turn!(packet, :back)
          elsif line.include? "#{$prefix}move"
            move!(packet)
          elsif line.include? "#{$prefix}take"
            take!(packet)
          elsif line.include? "#{$prefix}?_look_right"
            vm.puts "#{$prefix}!_#{look!(packet, :right)}" # vm.puts "#{$prefix}!_#{look(:right)}"
          elsif line.include? "#{$prefix}?_look_left"
            vm.puts "#{$prefix}!_#{look!(packet, :left)}"
          elsif line.include? "#{$prefix}?_look_back"
            vm.puts "#{$prefix}!_#{look!(packet, :back)}"
          elsif line.include? "#{$prefix}?_look_front"
            vm.puts "#{$prefix}!_#{look!(packet, :front)}"
          elsif line.include? "#{$prefix}?_look_here"
            vm.puts "#{$prefix}!_#{look!(packet, :here)}"
          elsif line.include? "#{$prefix}put_treasure"
            put!(packet, :treasure)
          elsif line.include? "#{$prefix}put_buoy"
            put!(packet, :buoy)
          elsif !line.chomp.empty?
            print!(packet, line, :log)
          end
        end
        send_packet(packet)
      end
    end
  end
end
