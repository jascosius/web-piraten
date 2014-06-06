# -*- encoding : utf-8 -*-

module GridSimulation


  class Ship
    attr_accessor :x, :y, :rotation
    def initialize(x, y, rotation)
      @x = x
      @y = y
      @rotation = rotation
    end
  end


  class  Grid
    def initialize(height, length, objects, ship)
      @grid= Hash.new(:nothing)
      @height = height
      @length = length
      @ship = Ship.new(ship['x'], ship['y'], ship['rotation'])
      objects.each{ |obj|
        x = obj['x']
        y = obj['y']
        case obj['name']
          when 'Monster'
            @grid[[x, y]] = :monster
          when 'Buoy'
            @grid[[x, y]] = :buoy
          when 'Treasure'
            @grid[[x, y]] = :treasure
          when 'Wave'
            @grid[[x, y]] = :wave
        end
      }
      puts @grid
      puts 'ich bin das grid'
    end
  end

  def move!(packet) # event: ship.move
    if !@is_simulation_done
      puts 'Move!'
      coord = get_next_position
      if coords_in_grid(coord)
        @ship['x'] = coord[0]
        @ship['y'] = coord[1]
        #WebsocketRails[:operations].trigger(:move, coord)
        packet[:operations] ||= []
        packet[:operations] << {name: 'move', return: coord}
      else
        @is_simulation_done = true
        packet[:operations] ||= []
        packet[:operations] << {name: 'move'}
        packet[:messages] ||= []
        packet[:messages] << {type: 'warning', message: 'Spielfeld verlassen'}
        #WebsocketRails[:operations].trigger(:done_error, 'Spielfeld verlassen')
      end
    end
  end

  def turn!(packet, direction) # event: ship.left
    if !@is_simulation_done
      case direction
        when :left
          puts 'Left!'
          @ship['rotation'] = (@ship['rotation'] - 1) % 4
        when :right
          puts 'Right!'
          @ship['rotation'] = (@ship['rotation'] + 1) % 4
        when :back
          puts 'Back!'
          @ship['rotation'] = (@ship['rotation'] + 2) % 4
      end
      packet[:operations] ||= []
      packet[:operations] << {name: 'turn', return: @ship['rotation']}
      #WebsocketRails[:operations].trigger(:turn, @ship['rotation'])
    end
  end

  def put!(packet, elem) # event: ship.put
    if !@is_simulation_done
      case elem
        when :buoy
          name = "Buoy"
        when :treasure
          name = "Treasure"
      end
      if !@objects.any? { |obj| obj['x'] == @ship['x'] && obj['y'] == @ship['y'] }
        object = {"name" => name, "x" => @ship['x'], "y" => @ship['y']}
        @objects.push(object)
        puts 'Put!'
        packet[:operations] ||= []
        packet[:operations] << {name: 'put', return: object}
        #WebsocketRails[:operations].trigger(:put, name)
      else
        packet[:operations] << {name: 'put', return: false}
        #WebsocketRails[:operations].trigger(:put, false)
      end
    end
  end

  def take!(packet) # event: ship.take
    if !@is_simulation_done
      coord = [@ship['x'], @ship['y']]
      puts @objects.to_s
      @objects.each_with_index { |obj, index|
        x = obj['x']
        y = obj['y']
        if  [x, y] == coord
          if obj['name'] == 'Treasure'
            @objects.delete_at index
            puts index
            #WebsocketRails[:operations].trigger(:take, index)
            puts 'Take!'
            packet[:operations] ||= []
            packet[:operations] << {name: 'take', return: index}
          end
        end
      }
    end
  end

  def look!(packet, direction) # event: ship.take
    if !@is_simulation_done
      puts 'Look!'
      coord = [@ship['x'], @ship['y']]
      next_coord = get_next_position
      rotate = @ship['rotation']
      puts rotate
      case direction
        when :front
          coord = next_coord
        when :back
          coord[0] -= next_coord[0] - coord[0]
          coord[1] -= next_coord[1] - coord[1]
        when :left
          case rotate
            when 3
              coord[0] += 1
            when 1
              coord[0] -= 1
            when 0
              coord[1] -= 1
            when 2
              coord[1] += 1
          end
        when :right
          case rotate
            when 3
              coord[0] -= 1
            when 1
              coord[0] += 1
            when 0
              coord[1] += 1
            when 2
              coord[1] -= 1
          end
      end
      if coords_in_grid(coord) == false
        look_obj = 'Border'
      else
        @objects.each { |obj|
          x = obj['x']
          y = obj['y']
          if  [x, y] == coord
            look_obj = obj['name'].to_s
            puts obj['name']
          end
        }
      end
      packet[:operations] ||= []
      packet[:operations] << {name: 'look', return: coord}
      #WebsocketRails[:operations].trigger(:look, coord)
    else
      look_obj='stop'
    end
    look_obj
  end

  def coords_in_grid(coord)
    if coord[0] >= 0 && coord[0] < @grid_size[0] && coord[1] >= 0 && coord[1]< @grid_size[1]
      true
    else
      false
    end
  end

  def get_next_position
    x = @ship['x']
    y = @ship['y']
    case @ship['rotation']
      when 0
        x += 1
      when 1
        y += 1
      when 2
        x -= 1
      when 3
        y -= 1
    end
    [x, y]
  end


end