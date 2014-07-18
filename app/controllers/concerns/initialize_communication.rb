# -*- encoding : utf-8 -*-
module InitializeCommunication

  if Rails.env.production?
    @@timeout = 20 #timeout time for the programm to execute
    @@port = 12340 #port to connect to the vm
    @@host = 'chevalblanc.informatik.uni-kiel.de' #host to connect to the vm
  else
    @@timeout = 20 #timeout time for the programm to execute
    @@port = 12340 #port to connect to the vm
    @@host = 'localhost' #host to connect to the vm
  end


  def start_simulation(code, tracing_vars)

    Thread.start do

      set_id
      read_json

      packet = {}

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
        exit_simulation!(packet, 'Ausführungszeit wurde überschritten.')
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
      exit_simulation!(packet, 'Ein interner Fehler ist aufgetreten.')
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
end