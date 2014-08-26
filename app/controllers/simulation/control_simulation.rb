# -*- encoding : utf-8 -*-

def new_line!(number)
  if @preprocessor.line_first
    @packet.send_packet
    @packet.add_line(number.to_i)
  else
    @packet.add_line(number.to_i)
    @packet.send_packet
  end
end

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

def print!(type, line)
  puts 'print'
  remove_prefix! line
  line = CGI::escapeHTML(line)
  @packet.add_message(type, line)
end

def debug!(tracing_vars, old_allocations, name_index, value)
  name = ''
  if tracing_vars.length > 0
    name = tracing_vars[name_index].chomp
  end
  remove_prefix! name
  remove_prefix! value

  old_allocations[name] ||= {}
  if old_allocations[name] != value
    old_allocations[name] = value
    @packet.add_allocation(name, value)
  end
end

def break!(direction)
  @packet.add_break(direction)
end

def remove_prefix!(string)
  string.gsub!("#{$prefix}_", '')
  string.gsub!($prefix, '')
end
