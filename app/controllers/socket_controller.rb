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

  def new_line(packet, number)
    send_packet(packet)
    packet.clear
    packet[:id] = @@id
    packet[:line] = number #line.split('!')[1].to_i
  end

  def search_and_execute_function(functions, array)
    functions.each do |key, value|
      if key.to_s == array[0]
        value.call(*array[1..-1])
        break
      end
    end
  end

  def communicate_with_vm(vm, packet, code, tracing_vars)
    old_allocations = {}

    functions = {:line => lambda { |number| new_line(packet, number) },
                 :debug => lambda { |name_index, *value| debug!(packet, tracing_vars, old_allocations, name_index.to_i, value.join('_')) }, #the value can contain _, with must be joint again
                 :move => lambda { @ship.move!(packet) },
                 :turn => lambda { |dir| @ship.turn!(packet, dir.to_sym) },
                 :put => lambda { |obj| @ship.put!(packet, obj.to_sym) },
                 :take => lambda { @ship.take!(packet) },
                 :look => lambda { |dir| @ship.look!(packet, dir.to_sym) },
                 :stderr => lambda { |*msg| print!(packet, :error, postprocess_error(msg.join('_'), code)) },
                 :stderrcompile => lambda { |*msg| print!(packet, :error, postprocess_error_compile(msg.join('_'), code)) },
                 :end => lambda { exit!(packet) },
                 :enderror => lambda { |*msg| exit!(packet, msg.join('_')) }}


    until connection_store[:is_simulation_done]
      line = vm.gets.chomp
      unless line.empty?
        array = line.split('_') #a command looks like $prefix_function_params or $prefix_?_function_params
        if array[0] == $prefix #is the line a command?
          if array[1] == '?' #is the command a question?
            vm.puts "response_#{search_and_execute_function(functions, array[2..-1])}" #when there is a ?, the vm expects a response
          else
            search_and_execute_function(functions, array[1..-1])
          end
        else
          print!(packet, :log, line) #without $prefix this must be a print from the user
        end
      end
    end
    vm.puts 'command_stop'
  end
end