# -*- encoding : utf-8 -*-
def set_id
  connection_store[:id] ||= 0
  connection_store[:id] += 1
end

# executes the function with the given parameters
def search_and_execute_function(functions, array)
  functions[array[0].to_sym].call(*array[1..-1])
end

# handle outputs of the vm
# send inputs to the vm
def communicate_with_vm(tracing_vars)
  communication_start = Time.now #Performance

  old_allocations = {}

  #lambda-function to send new commands to the vm
  send = lambda { |commands| @vm.puts proof_commands(commands) }

  @ship.send = send
  @ship.packet = @packet

  # functions that can be send by the vm
  functions = {:line => lambda { |number| new_line!(number) },
               :debug => lambda { |name_index, *value| debug!(tracing_vars, old_allocations, name_index.to_i, value.join('_')) }, #the value can contain _, with must be joint again
               :move => lambda { @ship.move! },
               :turn => lambda { |dir| @ship.turn!(dir.to_sym) },
               :put => lambda { |obj| @ship.put!(obj.to_sym) },
               :take => lambda { @ship.take! },
               :look => lambda { |dir| @ship.look!(dir.to_sym) },
               :break => lambda { |dir| break!(dir.to_sym) },
               :print => lambda { |type, *msg| result = @preprocessor.postprocess_print(send, type, msg.join('_'))
               unless result[:type] == :no
                 print!(result[:type], result[:message])
               end },
               :end => lambda { exit_simulation! },
               :enderror => lambda { |*msg| exit_simulation!(msg.join('_')) },
               :timings => lambda { |diff| PERFORMANCE_LOGGER.track(connection.id, :vm, diff.to_f) }}

  # get commands from the vm and handle them
  until connection_store[:is_simulation_done]
    line = @vm.gets.chomp
    line = line.force_encoding('utf-8')

    unless line.empty?
      array = line.split('_') #a command looks like VM_PREFIX_function_params or VM_PREFIX_?_function_params
      if array.length > 1 and array[0] == VM_PREFIX #is the line a command?
        if array[1] == '?' #is the command a question?
          @vm.puts(proof_commands([{:response => {:value => search_and_execute_function(functions, array[2..-1])}}])) #when there is a ?, the vm expects a response
        else
          search_and_execute_function(functions, array[1..-1])
        end
      else
        print!(:log, line) #without VM_PREFIX this must be a print from the user
      end
    end
  end
  @vm.puts(proof_commands([{:stop => {}}]))

  PERFORMANCE_LOGGER.track(connection.id, :communicate_with_vm, Time.now - communication_start)
end
