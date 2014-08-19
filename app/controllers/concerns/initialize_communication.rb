# -*- encoding : utf-8 -*-
module InitializeCommunication

  if Rails.env.production?
    @@timeout = 20 #timeout time for the programm to execute
    @@port = 12340 #port to connect to the vm
    @@host = 'chevalblanc.informatik.uni-kiel.de' #host to connect to the vm
  else
    @@timeout = 20 #timeout time for the programm to execute
    @@port = 12340 #port to connect to the vm
    @@host = 'localhost' #chevalblanc.informatik.uni-kiel.de' #host to connect to the vm #localhost
  end


  def start_simulation(code, tracing_vars)

    Thread.start do
      Perf::MeterFactory.instance.get(:vm).measure(:start_simulation_thread) {

      set_id
      Perf::MeterFactory.instance.get(:vm).measure(:read_json) {
        read_json
      }

      packet = {}

      connection_store[:is_simulation_done] = false

      Perf::MeterFactory.instance.get(:vm).measure(:initialize_timeout) {
        initialize_timeout(Thread.current, packet)
      }

      vm = Perf::MeterFactory.instance.get(:vm).measure(:initialize_vm) {
        initialize_vm(code, tracing_vars, packet)
      }
      Perf::MeterFactory.instance.get(:vm).measure(:communicate_with_vm) {
        communicate_with_vm(vm, packet, tracing_vars)
      }

      } # Performance
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

  def initialize_vm(code, tracing_vars, packet)

    begin
      #connect to TCPServer to execute the programm
      vm = TCPSocket.open(@@host, @@port)
    rescue
      $stderr.puts 'Could not connect to TCPSocket. Start ruby vm/vm/vm.rb development'
      exit_simulation!(packet, 'Ein interner Fehler ist aufgetreten.')
      send_packet(packet)
    else
      command = commands_for_vm(code,tracing_vars).to_json

      vm.puts command

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