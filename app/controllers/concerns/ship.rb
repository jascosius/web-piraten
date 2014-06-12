# -*- encoding : utf-8 -*-

class Ship
  attr_accessor :x_position, :y_position, :rotation

  def initialize(x, y, rotation, grid)
    @x_position = x
    @y_position = y
    @rotation = rotation
    @grid = grid
  end

  def turn!(packet, direction) # event: ship.left
    case direction
      when :left
        @rotation = (@rotation - 1) % 4
      when :right
        @rotation = (@rotation + 1) % 4
      else
        @rotation = (@rotation + 2) % 4
    end
    packet[:operations] ||= []
    packet[:operations] << {:name => 'turn', :return => @rotation}
  end

  def put!(packet, elem) # event: ship.put
    if @grid.grid[[@x_position, @y_position]] == :nothing
      @grid.grid[[@x_position, @y_position]] = elem
      packet[:operations] ||= []
      packet[:operations] << {:name => 'put', :return => {:name => elem, :x => @x_position, :y => @y_position}}
    else
      packet[:operations] ||= []
      packet[:operations] << {:name => 'put'}
      packet[:messages] ||= []
      packet[:messages] << {:type => 'warning', :message => 'Es ist kein Platz für ein weiteres Element'}

    end
  end

  def take!(packet) # event: ship.take
    elem = @grid.grid[[@x_position, @y_position]]
    if elem == :buoy || elem == :treasure
      @grid.grid[[@x_position, @y_position]] = :nothing
      packet[:operations] ||= []
      packet[:operations] << {:name => 'take', :return => {:name => elem, :x => @x_position, :y => @y_position}}
    else
      packet[:operations] ||= []
      packet[:operations] << {:name => 'take'}
      packet[:messages] ||= []
      packet[:messages] << {:type => 'warning', :message => 'Kein Objekt zum aufnehmen'}
    end
  end

  def look!(packet, direction)
    coord = [@x_position, @y_position]
    next_coord = get_next_position
    case direction
      when :front
        coord = next_coord
      when :back
        coord[0] -= next_coord[0] - @x_position
        coord[1] -= next_coord[1] - @y_position
      when :left
        case @rotation
          when 1
            coord[0] += 1
          when 3
            coord[0] -= 1
          when 0
            coord[1] -= 1
          else
            coord[1] += 1
        end
      when :right
        case @rotation
          when 1
            coord[0] -= 1
          when 3
            coord[0] += 1
          when 0
            coord[1] += 1
          else
            coord[1] -= 1
        end
      else
    end
    if coords_in_grid(coord)
      look_obj = @grid.grid[coord]
    else
      look_obj = :border
    end
    packet[:operations] ||= []
    packet[:operations] << {:name => 'look', :return => {:x => coord[0], :y => coord[1]}}
    look_obj
  end

  def move!(packet) # event: ship.move
    coord = get_next_position
    if coords_in_grid(coord)
      elem = @grid.grid[[coord[0], coord[1]]]
      case elem
        when :wave
          packet[:operations] ||= []
          packet[:operations] << {:name => 'move'}
          packet[:messages] ||= []
          packet[:messages] << {:type => 'warning', :message => 'Du wolltest in unruhige Gewässer fahren'}
        when :monster
          packet[:operations] ||= []
          packet[:operations] << {:name => 'exit'}
          packet[:messages] ||= []
          packet[:messages] << {:type => 'error', :message => 'Du bist auf einen Kraken gefahren'}
        else
          @x_position = coord[0]
          @y_position = coord[1]
          packet[:operations] ||= []
          packet[:operations] << {:name => 'move', :return => {:x => coord[0], :y => coord[1]}}
      end

    else
      packet[:operations] ||= []
      packet[:operations] << {:name => 'move'}
      packet[:messages] ||= []
      packet[:messages] << {:type => 'warning', :message => 'Du bist an das Ende der Welt gestoßen'}
    end
  end

  def coords_in_grid(coord)

    if coord[0] >= 0 && coord[0] < @grid.width && coord[1] >= 0 && coord[1]< @grid.height
      true
    else
      false
    end
  end

  def get_next_position
    x = @x_position
    y = @y_position
    case @rotation
      when 0
        x += 1
      when 1
        y += 1
      when 2
        x -= 1
      else
        y -= 1
    end
    [x, y]
  end

end