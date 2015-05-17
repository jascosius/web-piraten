# -*- encoding : utf-8 -*-

# initialize the grid on the server and start timeout for the simulation
# initialize the vm and start simulation
def start_simulation(tracing_vars)
  set_id
  read_json
  connection_store[:is_simulation_done] = false

  @packet = PacketHandler.new(connection_store[:id],lambda { |a, b| send_message(a,b)})
  initialize_timeout(Thread.current)
  @vm = initialize_vm

  communicate_with_vm(tracing_vars)
end

# kill the simulation after a while
def initialize_timeout(thread)
  #Thread to stop the execution after timeout time
  Thread.start(thread) do |thr|
    sleep(VM_TIMEOUT + 3) #add a little because normaly vm triggers timeout
    if thr.alive?
      puts 'kill'
      thr.kill
      exit_simulation!('Ausführungszeit wurde überschritten.')
      @packet.send_packet
    end
  end
end

# initialize the connection to the vm
# send the commends from the preprocessor
def initialize_vm

  begin
    #connect to TCPServer to execute the programm
    puts "Connecting to vm on #{VM_HOST}:#{VM_PORT}."
    vm = TCPSocket.open(VM_HOST, VM_PORT)
  rescue
    $stderr.puts 'Could not connect to TCPSocket. Start ruby vm/vm/vm.rb development'
    exit_simulation!('Ein interner Fehler ist aufgetreten.')
    @packet.send_packet
  else
    command = @preprocessor.commands_for_vm

    vm.puts proof_commands(command)

    vm
  end
end

# read json data and initialize the grid
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
