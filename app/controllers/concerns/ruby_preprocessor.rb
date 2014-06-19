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
  # if the user wants to trace specified variables. Furthermore it handles the
  # given code differently, when there is a correct case-statement by checking
  # the user's code of a valid one with a regular expression.
  def process_code(code_msg, vars)
    i=1
    codes = ''
    if code_msg =~ regex_verify_case_statement
      dont_skip_line = true
      code_msg.each_line do |s|
        if dont_skip_line
          codes += insert_line_number(i)
          codes += add_user_code(s, i)
          # match 'case casevariable # comment'
          if s =~ /^\s*case\s+(\w+|'\w*'|"\w*")\s*((#+.*\s*)*)?$/
            dont_skip_line = false
          else
            codes = insert_debug_information(codes, vars)
          end
          # match 'when case # comment'
        elsif s =~ /^\s*when\s+(\w+|'\w*'|"\w*")\s*(#+.*)?$/
          codes += add_user_code(s, i)
          dont_skip_line = true
        else
          codes += add_user_code(s, i)
        end
        i += 1
      end
    else # If there is no case-statement in the user's code, process code normally.
      code_msg.each_line do |s|
        codes += insert_line_number(i)
        codes += add_user_code(s, i)
        codes = insert_debug_information(codes, vars)
        i += 1
      end
    end
    insert_logic + codes + "\n"
  end


  # Regularexpression for validating case-statements, even those that have
  # comments and line breaks between them.
  def regex_verify_case_statement
    /^\s*case\s+(\w+|'\w*'|"\w*")\s*((#+.*\s*)*)?\s*when\s+(\w+|'\w*'|"\w*")\s*(#+.*)?$/
  end

  # Method to add the linenumber for linehighlighting in codemirror previous
  # to the user's line fo code.
  def insert_line_number(i)
    "#{$prefix}_line(#{i})\n"
  end

  # Adds the line of the user's code and a commment with the linenumber.
  def add_user_code(s, i)
    s.chomp + " # #{$prefix}_(#{i}#{$prefix}_)\n"
  end

  # Inserts the debug information for tracing the variables during simulation.
  # Also prohibits tracing of our predefined methods.
  def insert_debug_information(codes, vars)
    predefined_methods = %w(move turn put take look)
    vars.each_with_index do |variable, index|
      unless predefined_methods.include? variable
        codes += "if defined? #{variable} \n" +
            "  #{$prefix}_debug(#{variable}, #{index})\n" +
            "end\n"
      end
    end
    codes
  end

  def postprocess_error(line, code)

    #remove filepath
    index_begin = line.index('/') #filepath starts with /
    index_end = line.index(@filename) #filepath ends with filename
    if index_begin and index_end and index_begin < index_end #found a filepath?
      index_end += "#{@filename}".length #add the lenght of the filename to the end
      line.slice!(index_begin...index_end) #remove the filepath

      #change the linenumber
      index_line_end = line.index(':', index_begin+1) #find the : after the linenumber
      line_number = line[index_begin+1...index_line_end] #get the linenumber between the two :
      i = 1 #Set a counter
      new_line = '' #Set a result string
      code.each_line do |l| #search in the executed code for the right line. In every line is a comment with the original linenumber
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
    %Q[
# -*- encoding : utf-8 -*-
$stdout.sync = true
$stderr.sync = true
def #{$prefix}_debug(var, ind)
  puts "\n#{$prefix}_debug_\#{ind}_\#{var}"
end
def move
  puts "#{$prefix}_move"
end
def turn(dir = :back)
  case dir
    when :right then puts "#{$prefix}_turn_right"
    when :left then puts "#{$prefix}_turn_left"
    when :back then puts "#{$prefix}_turn_back"
    else raise(ArgumentError, "unknown argument")
  end
end
def put(elem = :buoy)
  case elem
    when :buoy then puts "#{$prefix}_put_buoy"
    when :treasure then puts "#{$prefix}_put_treasure"
    else raise(ArgumentError, "unknown argument")
  end
end
def take
  puts "#{$prefix}_take"
end
def #{$prefix}_line(i)
  puts "\n#{$prefix}_line_\#{i}"
end
def look(dir = :here)
  case dir
    when :right then puts "#{$prefix}_?_look_right"
    when :left then puts "#{$prefix}_?_look_left"
    when :here then puts "#{$prefix}_?_look_here"
    when :back then puts "#{$prefix}_?_look_back"
    when :front then puts "#{$prefix}_?_look_front"
    else raise(ArgumentError, "unknown argument")
  end
  ret = gets
  if ret.include? "buoy"
    return :buoy
  elsif ret.include? "monster"
    return :monster
  elsif ret.include? "treasure"
    return :treasure
  elsif ret.include? "wave"
    return :wave
  elsif ret.include? "border"
    return :border
  else
    return :nothing
  end
end
]
  end

end