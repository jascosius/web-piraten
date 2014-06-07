# -*- encoding : utf-8 -*-

class SocketController < WebsocketRails::BaseController
  $prefix = 'CkyUHZVL3q'
  @@timeout = 5 #timeout time for the programm to execute
  @@port = 12340 #port to connect to the vm
  @@host = 'localhost' #host to connect to the vm

  @@id ||= 0

  include Preprocessor
  include InitializeCommunication
  include Communication
  include GridSimulation
  include ControlSimulation

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

  def read_json
    grid = message[:grid]
    puts grid
    x = grid['width']
    y = grid['height']
    @grid_size = [x, y]
    objects = grid['objects'].to_a
    ship = grid['ship']
    @grid = Grid.new(x, y, objects)
    @ship = Ship.new(ship['x'], ship['y'], ship['rotation'], @grid)

  end


  #####initalize_communication TODO: move to initalize_communication.rb

  def set_id
    @@id += 1
    if @@id > 100000
      @@id = 0
    end
  end

  def receive_code

    tracing_vars = message[:vars]
    language = 'Ruby'
    code = preprocess_code(message[:code], language, tracing_vars)

    #add EOF to show Wrapper the end of the code
    code += "\n#{$prefix}_EOF\n"

    start_simulation(code, tracing_vars)

  end

  def start_simulation(code, tracing_vars)

    Thread.start do

      set_id
      read_json

      packet = Hash.new()
      packet[:id] = @@id

      connection_store[:is_simulation_done] = false

      initialize_timeout(Thread.current, packet)
      vm = initialize_vm(code, packet)
      communicate_with_vm(vm, packet, code, tracing_vars)

    end
  end

  def initialize_timeout(thread, packet)
    #Thread to stop the execution after timeout time
    Thread.start(thread) do |thr|
      sleep(@@timeout + 3) #add a little because normaly vm triggers timeout
      if thr.alive?
        puts 'kill'
        thr.kill
        exit!(packet, 'Ausführungszeit wurde überschritten.')
        send_packet(packet)
      end
    end
  end

  def initialize_vm(code, packet)

    begin
      #connect to TCPServer to execute the programm
      vm = TCPSocket.open(@@host, @@port)
    rescue
      puts 'Could not connect to TCPSocket. Start ruby app/vm/vm.rb'
      exit!(packet, 'Ein interner Fehler ist aufgetreten.')
      send_packet(packet)
    else

      #send commands to the server
      vm.puts preprocess_filename
      vm.puts preprocess_compile
      vm.puts preprocess_execute
      vm.puts preprocess_compile_error
      vm.puts preprocess_execute_error

      #send programmcode to the server
      vm.puts code

      vm
    end
  end


  ###communication TODO: move to communication.rb


  def remove_prefix!(string)
    string.gsub!("#{$prefix}_", '')
    string.gsub!($prefix, '')
  end

  def send_packet(packet)
    if packet.length > 1 #if the packet contains more then a id
      if connection_store[:is_simulation_done]
        puts 'Verbindung beendet'
      else
        puts packet
        WebsocketRails[:simulation].trigger(:step, packet)
      end
    end
  end

  def communicate_with_vm(vm, packet, code, tracing_vars)
    old_packet = {}
    loop do
      line = vm.gets
      if  line.include? "#{$prefix}_debug"
        debug!(packet, line, tracing_vars, old_packet)
      elsif line.include? "#{$prefix}_end_error"
        line.slice!("#{$prefix}_end_error")
        exit!(packet, line)
        break
      elsif line.include? "#{$prefix}_end"
        exit!(packet)
        break
      elsif line.include? "#{$prefix}_stderr_compile"
        line.slice!("#{$prefix}_stderr_compile")
        line = postprocess_error_compile(line, code)
        print!(packet, :error, line)
      elsif line.include? "#{$prefix}_stderr"
        line.slice!("#{$prefix}_stderr")
        line = postprocess_error(line, code)
        puts line
        print!(packet, :error, line)
      elsif line.include? "#{$prefix}_line"
        old_packet = packet.clone
        send_packet(packet)
        packet.clear
        packet[:id] = @@id
        packet[:line] = line.split('!')[1].to_i #send_line line.split('!')[1].to_i
      elsif line.include? "#{$prefix}_turn_right"
        @ship.turn!(packet, :right) #rotate_ship(:right)
      elsif line.include? "#{$prefix}_turn_left"
        @ship.turn!(packet, :left)
      elsif line.include? "#{$prefix}_turn_back"
        @ship.turn!(packet, :back)
      elsif line.include? "#{$prefix}_move"
        @ship.move!(packet)
      elsif line.include? "#{$prefix}_take"
        @ship.take!(packet)
      elsif line.include? "#{$prefix}_?_look_right"
        vm.puts "response_#{@ship.look!(packet, :right)}" # vm.puts "#{$prefix}!_#{look(:right)}"
      elsif line.include? "#{$prefix}_?_look_left"
        vm.puts "response_#{@ship.look!(packet, :left)}"
      elsif line.include? "#{$prefix}_?_look_back"
        vm.puts "response_#{@ship.look!(packet, :back)}"
      elsif line.include? "#{$prefix}_?_look_front"
        vm.puts "response_#{@ship.look!(packet, :front)}"
      elsif line.include? "#{$prefix}_?_look_here"
        vm.puts "response_#{@ship.look!(packet, :here)}"
      elsif line.include? "#{$prefix}_put_treasure"
        @ship.put!(packet, :treasure)
      elsif line.include? "#{$prefix}_put_buoy"
        @ship.put!(packet, :buoy)
      elsif !line.chomp.empty?
        print!(packet, :log, line)
      end
      if connection_store[:is_simulation_done]
        vm.puts 'command_stop'
      end
    end
    send_packet(packet)
  end

end