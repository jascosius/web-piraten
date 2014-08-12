module Communication

  @@id ||= 0 # Due to 'inheritance' through module including the class variable moved in this module.

  def set_id
    @@id += 1
    if @@id > 100000
      @@id = 0
    end
  end

  def remove_prefix!(string)
    string.gsub!("#{$prefix}_", '')
    string.gsub!($prefix, '')
  end

  def send_packet(packet)
    if packet.length > 0 #if the packet contains more then a id
      if connection_store[:is_simulation_done]
        puts 'Verbindung beendet'
      else
        packet[:id] = @@id
        puts packet
        WebsocketRails[:simulation].trigger(:step, packet)
      end
    end
    packet.clear
  end

  def new_line(packet, number)
    send_packet(packet)
    packet[:line] = number.to_i
  end

  def search_and_execute_function(functions, array)
    functions.each do |key, value|
      if key.to_s == array[0]
        return value.call(*array[1..-1])
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
                 :end => lambda { exit_simulation!(packet) },
                 :break => lambda { |dir| break!(packet,dir.to_sym)},
                 :enderror => lambda { |*msg| exit_simulation!(packet, msg.join('_')) }}

    until connection_store[:is_simulation_done]
      line = vm.gets.chomp
      line = line.force_encoding('utf-8')

      unless line.empty?
        array = line.split('_') #a command looks like $prefix_function_params or $prefix_?_function_params
        if array[0] == $prefix #is the line a command?
          if array[1] == '?' #is the command a question?
            vm.puts "response_#{search_and_execute_function(functions, array[2..-1])}" #when there is a ?, the vm expects a response
          else
            search_and_execute_function(functions, array[1..-1])
          end
        else
          print!(packet, :log, line)#line) #without $prefix this must be a print from the user
        end
      end
    end
    vm.puts 'command_stop'
  end
end