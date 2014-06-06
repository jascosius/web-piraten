# -*- encoding : utf-8 -*-

class SocketController < WebsocketRails::BaseController
  $prefix = 'CkyUHZVL3q_'
  $debugprefix = 'BvDpuDu4Be_'
  @@timeout = 5 #timeout time for the programm to execute
  @@port = 12340 #port to connect to the vm
  @@host = 'localhost' #host to connect to the vm

  @@id ||= 0

  include Preprocessor
  include GridSimulation

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


  def exit!(packet, type, line='')
    if !@is_simulation_done
      packet[:operations] ||= []
      packet[:operations] << {:name => 'exit', :return => type}
      if line != ''
        print!(packet, type, line) #add errormessages
      end
      @is_simulation_done = true
    end

  end

  def print!(packet, type, line)
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
      packet[:messages] << {:type => type, :message => line}
    end
  end

  def debug!(packet, line, tracing_vars, old_packet)
    index_begin = $debugprefix.length
    index_end = line.index('!')
    index = line[index_begin...index_end].to_i
    var_name = tracing_vars[index].chomp
    var_value = line[index_end+1..-1].chomp
    remove_prefix! var_name
    remove_prefix! var_value
    if old_packet[:allocations] and old_packet[:allocations][var_name]
      if old_packet[:allocations][var_name] != var_value
        packet[:allocations] ||= {}
        packet[:allocations][var_name] = var_value
      end
    else
      packet[:allocations] ||= {}
      packet[:allocations][var_name] = var_value
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
    @grid2 = Grid.new(x, y, @objects, @ship)
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

  def send_packet!(packet)
    if packet != {}
      puts packet
      WebsocketRails[:simulation_step].trigger(:packet, packet)
    end
  end

  def receive_code

    packet = Hash.new()
    old_packet = {}
    @@id += 1

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
            debug!(packet, line, tracing_vars, old_packet)
          elsif line.include? "#{$prefix}end_error"
            line.slice!("#{$prefix}end_error")
            exit!(packet, :error, line)
            break
          elsif line.include? "#{$prefix}end"
            exit!(packet, :successful)
            break
          elsif line.include? "#{$prefix}stderr_compile"
            line.slice!("#{$prefix}stderr_compile")
            line = postprocess_error_compile(line, code)
            puts line
            exit!(packet, :error, line)
          elsif line.include? "#{$prefix}stderr"
            line.slice!("#{$prefix}stderr")
            line = postprocess_error(line, code)
            puts line
            exit!(packet, :error, line)
          elsif line.include? "#{$prefix}line"
            old_packet = packet.clone
            send_packet!(packet)
            packet.clear
            packet[:id] = @@id
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
            print!(packet, :log, line)
          end
        end
        send_packet!(packet)
      end
    end
  end
end
