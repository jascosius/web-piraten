# -*- encoding : utf-8 -*-
# A packet which is send to the client
class PacketHandler

  def initialize(id, send_message)
    @id = id
    @send_message = send_message
    @packet = {}
  end

  # add the current line-number
  def add_line(line)
    @packet[:line] = line
  end

  # add a operation
  # send the packet when operation was added
  def add_operation(name, return_val = nil)
    @packet[:operations] ||= []
    if return_val == nil
      @packet[:operations] << {:name => name}
    else
      @packet[:operations] << {:name => name, :return => return_val}
    end
    send_packet
  end

  # add a message
  def add_message(type, message)
    @packet[:messages] ||= []
    @packet[:messages] << {:type => type, :message => message}
  end

  # add an allocation
  def add_allocation(name, value)
    @packet[:allocations] ||= {}
    @packet[:allocations][name] = value
  end

  # add a breakpoint or a jump information
  def add_break(direction)
    @packet[:break] ||= []
    @packet[:break] << {:type => direction}
  end

  # send the packet to the client, if it's not empty
  # clear the packet after sending
  def send_packet
    if @packet.length > 0
      @packet[:id] = @id
      puts @packet
      @send_message.call(:step, @packet)
    end
    @packet.clear
  end

end
