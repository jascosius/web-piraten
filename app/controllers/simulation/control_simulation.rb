# -*- encoding : utf-8 -*-

# when the execution reaches a new line, add this to the packed
# appending on the value of 'line_first' send the packet before or after adding the line-number
def new_line!(number)
  if @preprocessor.line_first
    @packet.send_packet
    @packet.add_line(number.to_i)
  else
    @packet.add_line(number.to_i)
    @packet.send_packet
  end
end

# tell the client and the server, that the execution has finished
def exit_simulation!(line='')
  @packet.send_packet
  if line != ''
    print!(:error, line) #add errormessages
  else
    print!(:log, 'Ausführung beendet!') #add endmessage
  end
  @packet.add_operation('exit')
  connection_store[:is_simulation_done] = true
end

#print a output by the user
def print!(type, line)
  remove_prefix! line
  line = CGI::escapeHTML(line)
  @packet.add_message(type, line)
end

# test if a traced variable has changed
# add the new value to the packet
def debug!(tracing_vars, old_allocations, name_index, value)
  name = ''
  if tracing_vars.length > 0
    if name_index.kind_of? Integer && name_index >= 0 && name_index < tracing_vars.length
      name = tracing_vars[name_index].chomp
    end
  end
  remove_prefix! name
  remove_prefix! value

  name = CGI::escapeHTML(name)
  value = CGI::escapeHTML(value)

  old_allocations[name] ||= {}
  if old_allocations[name] != value
    old_allocations[name] = value
    @packet.add_allocation(name, value)
  end
end

# add a break to the packet
def break!(direction)
  @packet.add_break(direction)
end

# remove the prefix in messages that will be printed to the user
# makes sure that the prefix is never printed
def remove_prefix!(string)
  string.gsub!("#{$prefix}_", '')
  string.gsub!($prefix, '')
end
