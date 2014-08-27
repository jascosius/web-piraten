# -*- encoding : utf-8 -*-
def proof_commands(array)
  error = ' Look at \'commands_for_vm.rb\' to see defined functions.'

  #these are all commands the vm can handle with
  default_functions = {:write_file => {:filename => nil, :permissions => 0744, :content => nil},
                       :execute => {:command => nil, :stdout => false, :stderr => 'error', :permissions => 'read-only'},
                       :exit => {:successful => true, :message => ''}}

  unless array.kind_of?(Array)
    $stderr.puts 'The argument for \'commands_for_vm.rb\' must be an array with hashes as values.'
    return
  end
  array.each do |item|
    unless default_functions[item.keys[0]]
      $stderr.puts "Command '#{item.keys[0].to_s}' is not defined." + error
      return
    end
    default_functions[item.keys[0]].each do |default_key, default_value|
      if item.values[0][default_key] == nil
        if default_value == nil
          $stderr.puts "Command '#{item.keys[0].to_s}' needs a value for '#{default_key}'." + error
          return
        end
        item.values[0][default_key] = default_value
      end
    end
  end
  array
end
