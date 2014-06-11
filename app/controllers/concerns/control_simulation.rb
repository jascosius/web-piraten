module ControlSimulation

  def exit_simulation!(packet, line='')
    send_packet(packet)
    packet.clear
    packet[:id] = 7 #@@id  TODO here is the little nasty bug
    packet[:operations] ||= []
    packet[:operations] << {:name => 'exit'}
    if line != ''
      print!(packet, :error, line) #add errormessages
    end
    send_packet(packet)
    connection_store[:is_simulation_done] = true
  end

  def print!(packet, type, line)
    remove_prefix! line
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

  def debug!(packet, tracing_vars, old_allocations, name_index, value)
    name = tracing_vars[name_index].chomp
    remove_prefix! name
    remove_prefix! value

    old_allocations[name] ||= {}
    if old_allocations[name] != value
      old_allocations[name] = value
      packet[:allocations] ||= {}
      packet[:allocations][name] = value
    end
  end
end