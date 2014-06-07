# -*- encoding : utf-8 -*-
class RubyPreprocessor < BasePreprocessor

  attr :filename
  attr :compile
  attr :execute
  attr :compile_error
  attr :execute_error

  def initialize(attribut)
    super(attribut)
    @filename = "#{$prefix}_code.rb"
    @compile = ''
    @execute = "ruby $PATH$/#{$prefix}_code.rb" #$PATH$ will be replaced
    @compile_error = ''
    @execute_error = ''
  end

  # Method that preprocesses the given code and includes the debug information
  # if the user wants to trace specified variables.
  def process_code(code_msg, vars)
    # Array that contains predefined methodnames to be ignored by the debugger
    predefined_methods = ['move', 'turn', 'put', 'take', 'look']
    i=1
    codes = ''
    code_msg.each_line do |s|
      codes += "#{$prefix}_line(#{i})\n" # add linefunction for linehighlighting
      vars.each_with_index do |variable, index|
        unless predefined_methods.include? variable
          codes += "if defined? #{variable} \n" +
              "  #{$prefix}_debug(#{variable}, #{index})\n" +
              "end\n"
        end
      end
      codes += s.chomp + " # #{$prefix}_(#{i+1}#{$prefix}_)\n" # add linenumber in comment
      i += 1
    end
    insert_logic + codes + "\n"
  end

  def postprocess_error(line, code)

    #remove filepath
    index_begin = line.index('/') #filepath starts with /
    index_end = line.index(@filename) #filepath ends with filename
    if index_begin and index_end and index_begin < index_end #found a filepath?
      index_end += "#{@filename}".length #add the lenght of the filename to the end
      line.slice!(index_begin...index_end) #remove the filepath

      #chance the linenumber
      index_line_end = line.index(':', index_begin+1) #find the : after the linenumber
      line_number = line[index_begin+1...index_line_end] #get the linenumber between the two :
      i = 1 #Set a counter
      new_line = '' #Set a result string
      code.each_line() do |l| #search in the executed code for the right line. In every line is a comment with the original linenumber
        if i == line_number.to_i #find the line from the errormessage
          line_begin=l.index("#{$prefix}_(") #find the begin of the original linenumber in the comment
          line_end=l.index("#{$prefix}_)") #find the end of the original linenumber in the comment
          if line_begin and line_end #found something?
            new_line = l[line_begin+"#{$prefix}_(".length...line_end] #Set the new linenumber to the number in the comment
          end
        end
        i += 1
      end
      line.slice!(index_begin+1...index_line_end) #remove the old linenumber from the error

      if new_line == '' #is there a result for the new linenumber?
        line.slice!(index_begin..index_begin+1) #remove the : around the old number
      else
        line = line.insert(index_begin+1, new_line) #add the new linenumber to the error
        line = line.insert(index_begin, 'line') #add a line to the error instead of the filepath
      end
    end
    line
  end

  def postprocess_error_compile(lang, code)

  end

  # A method that stores the language- and ship-logic for Ruby that's put in the
  # code of the user to get the ship moving and so on.
  def insert_logic
    "# -*- encoding : utf-8 -*-\n" +
        "$stdout.sync = true\n" +
        "$stderr.sync = true\n" +
        "def #{$prefix}_debug(var, ind)\n" +
        "  puts \"\n#{$prefix}_debug_\#{ind}!\#{var}\" \n" +
        "end\n" +
        "def move\n" +
        "  puts \"#{$prefix}_move\"\n" +
        "end\n" +
        "def turn(dir = :back)\n" +
        "  case dir\n"+
        "    when :right then puts \"#{$prefix}_turn_right\"\n" +
        "    when :left then puts \"#{$prefix}_turn_left\"\n" +
        "    when :back then puts \"#{$prefix}_turn_back\"\n" +
        "    else raise(ArgumentError, \"unknown argument\")\n" +
        "  end\n"+
        "end\n" +
        "def put(elem = :buoy)\n" +
        "  case elem\n"+
        "    when :buoy then puts \"#{$prefix}_put_buoy\"\n"+
        "    when :treasure then puts \"#{$prefix}_put_treasure\"\n"+
        "    else raise(ArgumentError, \"unknown argument\")\n" +
        "  end\n"+
        "end\n" +
        "def take\n" +
        "  puts \"#{$prefix}_take\"\n" +
        "end\n" +
        "def #{$prefix}_line(i)\n" +
        "  puts \"\\n#{$prefix}_line!\#{i}\"\n" +
        "end\n" +
        "def look(dir = :here)\n" +
        "  case dir\n" +
        "    when :right then puts \"#{$prefix}_?_look_right\"\n" +
        "    when :left then puts \"#{$prefix}_?_look_left\"\n" +
        "    when :here then puts \"#{$prefix}_?_look_here\"\n" +
        "    when :back then puts \"#{$prefix}_?_look_back\"\n" +
        "    when :front then puts \"#{$prefix}_?_look_front\"\n"+
        "    else raise(ArgumentError, \"unknown argument\")\n" +
        "  end\n" +
        "  ret = gets\n" +
        "  if ret.include? \"buoy\"\n" +
        "    return :buoy\n" +
        "  elsif ret.include? \"monster\"\n" +
        "    return :monster\n" +
        "  elsif ret.include? \"treasure\"\n" +
        "    return :treasure\n" +
        "  elsif ret.include? \"wave\"\n" +
        "    return :wave\n" +
        "  elsif ret.include? \"border\"\n" +
        "    return :border\n" +
        "  else\n" +
        "    return :nothing\n" +
        "  end\n" +
        "end\n\n"
  end

end