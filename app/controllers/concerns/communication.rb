module Communication

  def set_id
    connection_store[:id] ||= 0
    connection_store[:id] += 1
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
        packet[:id] = connection_store[:id]

        #send every operation in a single packet
        if packet[:operations]
          while packet[:operations].size > 1
            new_packet = {}
            new_packet[:operations] = [packet[:operations][0]]
            if packet[:line]
              new_packet[:line] = packet[:line]
            end
            new_packet[:id] = packet[:id]
            puts new_packet
            # send_message :step, new_packet
            packet[:operations].slice!(0)
          end
        end
        #remove till here to send the whole packet without splitting

        puts packet
        # send_message :step, packet
      end
    end
    packet.clear
  end

  def new_line(packet, number)
    if line_first
      send_packet(packet)
      packet[:line] = number.to_i
    else
      packet[:line] = number.to_i
      send_packet(packet)
    end
  end

  def search_and_execute_function(functions, array)
    functions[array[0].to_sym].call(*array[1..-1])
  end

  def communicate_with_vm(vm, packet, tracing_vars)
    communication_start = Time.now #Performance

    old_allocations = {}

    send = lambda {|commands| vm.puts proof_commands(commands).to_json}
    functions = {:line => lambda { |number| new_line(packet, number) },
                 :debug => lambda { |name_index, *value| debug!(packet, tracing_vars, old_allocations, name_index.to_i, value.join('_')) }, #the value can contain _, with must be joint again
                 :move => lambda { @ship.move!(packet) },
                 :turn => lambda { |dir| @ship.turn!(packet, dir.to_sym) },
                 :put => lambda { |obj| @ship.put!(packet, obj.to_sym) },
                 :take => lambda { @ship.take!(packet) },
                 :look => lambda { |dir| @ship.look!(packet, dir.to_sym) },
                 :break => lambda { |dir| break!(packet, dir.to_sym) },
                 :print => lambda { |type, *msg| result = postprocess_print(send, type, msg.join('_')); unless result[:type] == :no; print!(packet, result[:type], result[:message]) end } ,
                 :end => lambda { exit_simulation!(packet) },
                 :enderror => lambda { |*msg| exit_simulation!(packet, msg.join('_')) },
                 :timings => lambda { |diff| PERFORMANCE_LOGGER.track(connection.id, :vm, diff.to_f)}}

    until connection_store[:is_simulation_done]
      line = vm.gets.chomp
      line = line.force_encoding('utf-8')

      unless line.empty?
        array = line.split('_') #a command looks like $prefix_function_params or $prefix_?_function_params
        if array[0] == $prefix #is the line a command?
          if array[1] == '?' #is the command a question?
            vm.puts([{:response => search_and_execute_function(functions, array[2..-1])}].to_json) #when there is a ?, the vm expects a response
          else
            search_and_execute_function(functions, array[1..-1])
          end
        else
          print!(packet, :log, line) #without $prefix this must be a print from the user
        end
      end
    end
    vm.puts([{:stop => nil}].to_json)

    PERFORMANCE_LOGGER.store :communicate_with_vm, communication_start, Time.now
    # PERFORMANCE_LOGGER.track(connection.id, :communicate_with_vm, Time.now - communication_start)
  end
end