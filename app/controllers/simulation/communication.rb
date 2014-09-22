# -*- encoding : utf-8 -*-
def set_id
  connection_store[:id] ||= 0
  connection_store[:id] += 1
end

def search_and_execute_function(functions, array)
  functions[array[0].to_sym].call(*array[1..-1])
end

def communicate_with_vm(tracing_vars)
  communication_start = Time.now #Performance

  old_allocations = {}

  send = lambda { |commands|
    # start_proof_commands = Time.now
    commands = proof_commands(commands).to_json
    # PERFORMANCE_LOGGER.store :proof_commands, start_proof_commands, Time.now
    PERFORMANCE_LOGGER.store :sendTo_vm, connection_store[:incoming], Time.now
    @vm.puts commands }
  @ship.send = send
  @ship.packet = @packet

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
               :timings => lambda { |name, diff| PERFORMANCE_LOGGER.store name.to_sym, 0, diff.to_f }}

  until connection_store[:is_simulation_done]
    # perf = Time.now
    line = @vm.gets.chomp
    if line == "PING!"
      @vm.puts "PONG!"
      next
    end
    perf = Time.now
    # PERFORMANCE_LOGGER.store :vm_get_line, perf, Time.now
    line = line.force_encoding('utf-8')

    # perf = Time.now
    unless line.empty?
      array = line.split('_') #a command looks like $prefix_function_params or $prefix_?_function_params
      if array[0] == $prefix #is the line a command?
        if array[1] == '?' #is the command a question?
          @vm.puts([{:response => {:value => search_and_execute_function(functions, array[2..-1])}}].to_json) #when there is a ?, the vm expects a response
          PERFORMANCE_LOGGER.store :vm_read_look, perf, Time.now
        else
          search_and_execute_function(functions, array[1..-1])
        end
      else
        print!(:log, line) #without $prefix this must be a print from the user
      end
    end
    # PERFORMANCE_LOGGER.store :vm_zeile_abarbeiten, perf, Time.now
  end
  @vm.puts([{:stop => {}}].to_json)

  PERFORMANCE_LOGGER.store :communicate_with_vm, communication_start, Time.now
end
