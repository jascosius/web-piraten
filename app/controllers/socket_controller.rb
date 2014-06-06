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

  def send_packet(packet)
    if packet != {}
      puts packet
      WebsocketRails[:simulation].trigger(:step, packet)
    end
  end

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
    code += "\n#{$prefix}EOF\n"

    start_simulation(code, tracing_vars)

  end

  def start_simulation(code, tracing_vars)

    Thread.start do

      set_id
      read_JSON

      packet = Hash.new()
      @is_simulation_done = false

      initialize_timeout(Thread.current, packet)
      vm = initialize_vm(code, packet)
      communicate_with_vm(vm,packet,code,tracing_vars)

    end
  end

  def initialize_timeout(thread, packet)
    #Thread to stop the execution after timeout time
    Thread.start(thread) do |thr|
      sleep(@@timeout)
      if thr.alive?
        puts 'kill'
        exit!(packet, 'Ausführungszeit wurde überschritten.')
        thr.kill
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

  def communicate_with_vm(vm,packet,code,tracing_vars)
    old_packet = {}
    loop do
      line = vm.gets
      if  line.include? "#{$debugprefix}"
        debug!(packet, line, tracing_vars, old_packet)
      elsif line.include? "#{$prefix}end_error"
        line.slice!("#{$prefix}end_error")
        exit!(packet, line)
        break
      elsif line.include? "#{$prefix}end"
        exit!(packet)
        break
      elsif line.include? "#{$prefix}stderr_compile"
        line.slice!("#{$prefix}stderr_compile")
        line = postprocess_error_compile(line, code)
        print!(packet, :error, line)
      elsif line.include? "#{$prefix}stderr"
        line.slice!("#{$prefix}stderr")
        line = postprocess_error(line, code)
        puts line
        print!(packet, :error, line)
      elsif line.include? "#{$prefix}line"
        old_packet = packet.clone
        send_packet(packet)
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
    send_packet(packet)
  end

end
