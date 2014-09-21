# -*- encoding : utf-8 -*-
if Rails.env.production?
  TIMEOUT = 20 #timeout time for the programm to execute
  PORT = 12340 #port to connect to the vm
  HOST = 'chevalblanc.informatik.uni-kiel.de' #host to connect to the vm
else
  TIMEOUT = 20 #timeout time for the programm to execute
  PORT = 12340 #port to connect to the vm
  HOST = 'localhost' #chevalblanc.informatik.uni-kiel.de' #host to connect to the vm #localhost
end


def start_simulation(tracing_vars)
  set_id
  read_json
  connection_store[:is_simulation_done] = false

  @packet = PacketHandler.new(connection_store[:id],lambda { |a, b|
    unless connection_store[:first_packet_send]
      puts "first packet!"
      connection_store[:first_packet_send] = true

      puts "first packet2!"
      PERFORMANCE_LOGGER.store :first_packet, connection_store[:incoming], Time.now
    end
    send_message(a,b)
  })
  initialize_timeout(Thread.current)
  @vm = initialize_vm

  communicate_with_vm(tracing_vars)
end

def initialize_timeout(thread)
  #Thread to stop the execution after timeout time
  Thread.start(thread) do |thr|
    sleep(TIMEOUT + 3) #add a little because normaly vm triggers timeout
    if thr.alive?
      puts 'kill'
      thr.kill
      exit_simulation!('Ausführungszeit wurde überschritten.')
      @packet.send_packet
    end
  end
end

def initialize_vm

  begin
    #connect to TCPServer to execute the programm
    open_server = Time.now
    vm = TCPSocket.open(HOST, PORT)
    PERFORMANCE_LOGGER.store :tcp_setup, open_server, Time.now
  rescue
    $stderr.puts 'Could not connect to TCPSocket. Start ruby vm/vm/vm.rb development'
    exit_simulation!('Ein interner Fehler ist aufgetreten.')
    @packet.send_packet
  else

    command = @preprocessor.commands_for_vm.to_json

    command = [{:time => {:time => Time.now.to_i }}].to_json

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
