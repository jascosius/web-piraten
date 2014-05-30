# -*- encoding : utf-8 -*-
class RubyPreprocessor < BasePreprocessor

  attr :filename
  attr :execute
  attr :compile

  def initialize(attribut)
    super(attribut)
    @filename = 'code.rb'
    @execute = 'ruby $FILE$' #$FILE$ will be replaced
    @compile = ''
  end

  def process_code(code_msg)
    i=0
    codes = ''
    code_msg.each_line do |s|
      #      # remove \n   #add linenumber in commend          #add linefunction for linehighlighting
      codes += s.chomp + " # #{$prefix}(#{i+1}#{$prefix})\n" + "#{$prefix}line(#{i})\n"
      i += 1
    end
    insert_logic + codes + "\n"
  end

  def postprocess_error(line, code, file)

    #remove filepath
    index_begin = line.index('/') #filepath starts with /
    index_end = line.index($prefix+file) #filepath ends with filename
    if index_begin and index_end and index_begin < index_end #found a filepath?
      index_end += "#{$prefix+file}".length #add the lenght of the filename to the end
      line.slice!(index_begin...index_end) #remove the filepath

      #chance the linenumber
      index_line_end = line.index(':', index_begin+1) #find the : after the linenumber
      line_number = line[index_begin+1...index_line_end] #get the linenumber between the two :
      i = 1 #Set a counter
      new_line = '' #Set a result string
      code.each_line() do |l| #search in the executed code for the right line. In every line is a comment with the original linenumber
        if i == line_number.to_i #find the line from the errormessage
          line_begin=l.index("#{$prefix}(") #find the begin of the original linenumber in the comment
          line_end=l.index("#{$prefix})") #find the end of the original linenumber in the comment
          if line_begin and line_end #found something?
            new_line = l[line_begin+"#{$prefix}(".length...line_end] #Set the new linenumber to the number in the comment
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

  def debug_code(code_msg, vars)
    i=0
    code = ''
    code_msg << "\n"
    code_msg.each_line do |s|
      code += s + "line(#{i})\n" #TODO: add the json object for tracing variables
      i += 1
    end
    insert_logic + code + "\n"
  end

  # A method that stores the language- and ship-logic for Ruby that's put in the
  # code of the user to get the ship moving and so on.
  def insert_logic
    "# -*- encoding : utf-8 -*-\n" +
        "$stdout.sync = false\n" +
        "$stderr.sync = false\n" +
        "def move\n" +
        "  puts \"#{$prefix}move\"\n" +
        "end\n" +
        "def turn(dir = :over)\n" +
        "  case dir\n"+
        "    when :right then puts \"#{$prefix}turn_right\"\n" +
        "    when :left then puts \"#{$prefix}turn_left\"\n" +
        "    when :over then puts \"#{$prefix}turn_over\"\n" +
        "  end\n"+
        "end\n" +
        "def put\n" +
        "  puts \"#{$prefix}put\"\n" +
        "end\n" +
        "def #{$prefix}line(i)\n" +
        "  puts \"\\n#{$prefix}line!\#{i}\"\n" +
        "end\n" +
        "def look(dir = :here)\n" +
        "  case dir\n" +
        "    when :right then puts \"#{$prefix}?_look_right\"\n" +
        "    when :left then puts \"#{$prefix}?_look_left\"\n" +
        "    when :here then puts \"#{$prefix}?_look_here\"\n" +
        "    when :back then puts \"#{$prefix}?_look_back\"\n" +
        "    when :front then puts \"#{$prefix}?_look_front\"\n"+
        "  end\n" +
        "  ret = gets\n" +
        "  if ret.include? \"#{$prefix}!_Buoy\"\n" +
        "    return :buoy\n" +
        "  elsif ret.include? \"#{$prefix}!_Monster\"\n" +
        "    return :monster\n" +
        "  elsif ret.include? \"#{$prefix}!_Treasure\"\n" +
        "    return :treasure\n" +
        "  elsif ret.include? \"#{$prefix}!_Wave\"\n" +
        "    return :wave\n" +
        "  else\n" +
        "    return :nothing\n" +
        "  end\n" +
        "end\n\n"
  end

end