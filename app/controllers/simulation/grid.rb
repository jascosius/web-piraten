# -*- encoding : utf-8 -*-

class Grid
  attr_accessor :height, :width, :grid

  def initialize(width, height, objects)
    @grid = Hash.new(:nothing)
    @height = height
    @width = width
    objects.each { |obj|
      x = obj['x']
      y = obj['y']
      case obj['name']
        when 'monster'
          @grid[[x, y]] = :monster
        when 'buoy'
          @grid[[x, y]] = :buoy
        when 'treasure'
          @grid[[x, y]] = :treasure
        else
          @grid[[x, y]] = :wave

      end
    }
  end
end