class RubyPreprocessor < BasePreprocessor

  # A constant that stores the ship-logic for Ruby that's put in the code of the user to get the ship moving.
  # Looks like this:
  #  def move
  #    puts "move"
  #  end
  #  def turnRight
  #    puts "turnRight"
  #  end
  #  def turnLeft
  #    puts "turnLeft"
  #  end
  #  def addBuoy
  #    puts "addBuoy"
  #  end
  #  def line(i)
  #    puts "\nline?#{i}"
  #  end
  LANGUAGE_LOGIC = "STDOUT.sync = true\n"
  SHIP_LOGIC = "def move\n  puts \"#{$prefix}move\"\nend\ndef turnRight\n  puts \"#{$prefix}turnRight\"\nend\ndef turnLeft\n  puts \"#{$prefix}turnLeft\"\nend\ndef put\n  puts \"#{$prefix}put\"\nend\ndef line(i)\n  puts \"\\n#{$prefix}line!\#{i}\"\nend\n\n"

  def initialize(attribut)
    super(attribut)
  end

  def process_code(code_msg)
    i=0
    codes = ''
    code_msg << "\n"
    code_msg.each_line do |s|
      codes += s + "line(#{i})\n"
      i += 1
    end
    LANGUAGE_LOGIC + SHIP_LOGIC + codes + "\n"
  end

  def debug_code(code_msg, vars)
    i=0
    code = ''
    code_msg << "\n"
    code_msg.each_line do |s|
      code += s + "line(#{i})\n" #TO DO: add the json object for tracing variables
      i += 1
    end
    LANGUAGE_LOGIC + SHIP_LOGIC + code + "\n"
  end

end