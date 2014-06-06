module ControlSimulation

  def exit!(packet, line='')
    if !@is_simulation_done
      packet[:operations] ||= []
      packet[:operations] << {:name => 'exit'}
      if line != ''
        print!(packet, :error, line) #add errormessages
      end
      @is_simulation_done = true
    end

  end

  def print!(packet, type, line)
    if !@is_simulation_done
      remove_prefix! line
      puts line
      line = CGI::escapeHTML(line)
      new_line = ''
      line.each_char do |c|
        if c == ' '
          new_line += '&nbsp;'
        elsif c == "\t"
          new_line += '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'
        else
          new_line += c
        end
      end
      packet[:messages] ||= []
      packet[:messages] << {:type => type, :message => line}
    end
  end

  def debug!(packet, line, tracing_vars, old_packet)
    index_begin = $debugprefix.length
    index_end = line.index('!')
    index = line[index_begin...index_end].to_i
    var_name = tracing_vars[index].chomp
    var_value = line[index_end+1..-1].chomp
    remove_prefix! var_name
    remove_prefix! var_value
    if old_packet[:allocations] and old_packet[:allocations][var_name]
      if old_packet[:allocations][var_name] != var_value
        packet[:allocations] ||= {}
        packet[:allocations][var_name] = var_value
      end
    else
      packet[:allocations] ||= {}
      packet[:allocations][var_name] = var_value
    end
  end

end