# -*- encoding : utf-8 -*-
class PacketHandler

  def initialize(id, send_message)
    @id = id
    @send_message = send_message
    @packet = {}
  end

  def add_line(line)
    @packet[:line] = line
  end

  def add_operation(name, return_val = nil)
    @packet[:operations] ||= []
    if return_val == nil
      @packet[:operations] << {:name => name}
    else
      @packet[:operations] << {:name => name, :return => return_val}
    end
    send_packet
  end

  def add_message(type, message)
    @packet[:messages] ||= []
    @packet[:messages] << {:type => type, :message => message}
  end

  def add_allocation(name, value)
    @packet[:allocations] ||= {}
    @packet[:allocations][name] = value
  end

  def add_break(direction)
    if @packet[:break]
      send_packet
    end
    @packet[:break] ||= []
    @packet[:break] << {:type => direction}
  end

  def send_packet
    if @packet.length > 0
      @packet[:id] = @id
      puts @packet
      @send_message.call(:step, @packet)
    end
    @packet.clear
  end

end
