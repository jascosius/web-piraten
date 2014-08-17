module CommandsForVm

  def proof_hash(hash)
    error = ' Look at \'commands_for_vm.rb\' to see defined functions.'

    #these are all commands the vm can handle with
    default_functions = {:write_file => {:filename => nil, :permissions => 0744, :content => nil},
    :read_file => {:filename => nil, :path => '.', :prefix => '' },
    :execute => {:command => nil, :stdout_prefix => 'log', :stderr_prefix => 'error', }}

    hash.each do |key, value|
      unless default_functions[key]
        $stderr.puts "Command '#{key.to_s}' is not defined." + error
        return
      end
      default_functions[key].each do |default_key, default_value|
        unless value[default_key]
          if default_value == nil
            $stderr.puts "Command '#{key.to_s}' needs a value for '#{default_key}'." + error
            return
          end
          value[default_key] = default_value
        end
      end
    end
    puts 'Das Ergebnis: '
    puts hash
    puts 'Ende.'
    hash
  end
end