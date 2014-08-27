# -*- encoding : utf-8 -*-
class RubyPreprocessor

  require 'preprocessor/ruby/regular_expressions'
  require 'preprocessor/ruby/processing_tools'

  attr :line_first

  def initialize(code, tracing_vars)
    @operationlist = []
    @line_first = true
    @filename = "#{$prefix}_code.rb"
    @syntaxflag = true
    @code = code
    @process_code = process_code(code, tracing_vars)
  end

  def commands_for_vm
    [{:write_file => {:filename => @filename, :content => @code}},
     {:execute => {:command => "ruby -c #{@filename}", :stdout => 'checksuccess', :stderr => 'checkerror'}}]
  end

  # Method that processes the given code and includes the debug information
  # if the user wants to trace specified variables. Furthermore it handles the
  # given code differently, when there are multline strings or correct case-
  # statements. This'll be verified by checking the user's code with regular
  # expressions.
  def process_code(code_msg, vars)
    i=1
    codes = ''
    bools = {}
    bools[:single_q] = false
    bools[:double_q] = false
    bools[:dont_skip_line] = true
    if code_msg =~ regex_verify_case_statement
      bools[:no_multiline_case_statement] = true
      bools[:multiline_string] = false
      bools[:found_case] = false
      bools[:found_when] = false
      code_msg.each_line do |s|
        codes += insert_line_number(i, bools[:dont_skip_line])
        bools = case_block_processing(s, bools)
        codes += add_user_codeline(s, i, bools[:dont_skip_line])
        codes = insert_debug_information(codes, vars, bools[:dont_skip_line])
        i += 1
      end
    elsif code_msg =~ regex_verify_multiline_string
      code_msg.each_line do |s|
        codes += insert_line_number(i, bools[:dont_skip_line])
        bools = multiline_processing(s, bools)
        codes += add_user_codeline(s, i, bools[:dont_skip_line])
        codes = insert_debug_information(codes, vars, bools[:dont_skip_line])
        i += 1
      end
    else
      code_msg.each_line do |s|
        codes += insert_line_number(i, true)
        codes += add_user_codeline(s, i, true)
        codes = insert_debug_information(codes, vars, true)
        i += 1
      end
    end
    insert_logic + codes + "\n"
  end



  # Method to add the linenumber for linehighlighting in codemirror. Previous
  # to the user's line of code.
  def insert_line_number(i, dont_skip_line)
    dont_skip_line ? "#{$prefix}_line(#{i})\n" : ''
  end

  # Adds the line of the user's code and a commment with the linenumber, doesn't add
  # a comment if it's processing a multiline string.
  def add_user_codeline(s, i, no_multiline_string)
    if s=~ /\bdef\b/ || s=~/\bclass\b/ #def class
      @operationlist.unshift(:defClass)
      no_multiline_string ? s.chomp + "; break_point(:down)" + " # #{$prefix}_(#{i}#{$prefix}_)\n" : s
    elsif s=~ /\bdo\b/ || s=~ /\bwhile\b/ || s=~ /\bif\b/ #do while if
      @operationlist.unshift(:doWhileIf)
      no_multiline_string ? "break_point(:down); " + s.chomp + " # #{$prefix}_(#{i}#{$prefix}_)\n" : s
    elsif s=~ /\bend\b/ #end
      op = @operationlist.shift
      if op == :defClass
        no_multiline_string ? "break_point(:up); " + s.chomp + " # #{$prefix}_(#{i}#{$prefix}_)\n" : s
      else
        no_multiline_string ? s.chomp + " ; break_point(:up)" + " # #{$prefix}_(#{i}#{$prefix}_)\n" : s
      end
    else
      no_multiline_string ? s.chomp + " # #{$prefix}_(#{i}#{$prefix}_)\n" : s
    end

  end

  # Inserts the debug information for tracing the variables during simulation.
  # Also prohibits tracing of our predefined methods.
  def insert_debug_information(codes, vars, dont_skip_line)
    if dont_skip_line
      vars.each_with_index do |variable, index|
        codes += "if defined?(#{variable}) == 'local-variable'\n" +
            "  #{$prefix}_debug(#{variable}, #{index})\n" +
            "end\n"
      end
    end
    codes
  end

  def postprocess_print(send, type, line)
    if type == 'checksuccess'
      send.call([{:write_file => {:filename => @filename, :content => @process_code}}, {:execute => {:command => "ruby #{@filename}"}}, {:exit => {}}])
      {:type => :no}
    elsif type == 'checkerror'
      if @syntaxflag
        send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
        @syntaxflag = false
      end
      return {:type => :error, :message => line}
    else
      postprocess_execute(line)
    end
  end

  def postprocess_execute(line)
    #remove filepath
    index_begin = line.index(@filename) #filepath ends with filename
    if index_begin
      index_end = index_begin + "#{@filename}".length #add the lenght of the filename to the end
      line.slice!(index_begin...index_end) #remove the filepath

      #change the linenumber
      index_line_end = line.index(':', index_begin+1) #find the : after the linenumber
      line_number = line[index_begin+1...index_line_end] #get the linenumber between the two :
      i = 1 #Set a counter
      new_line = '' #Set a result string
      @process_code.each_line do |l| #search in the executed code for the right line. In every line is a comment with the original linenumber
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
    {:type => :error, :message => line}
  end

  # A method that stores the language- and ship-logic for Ruby that's put in the
  # code of the user to get the ship moving and so on.
  def insert_logic
    %Q[
# -*- encoding : utf-8 -*-
$stdout.sync = true
$stderr.sync = true
def #{$prefix}_debug(var, ind)
  puts "\n#{$prefix}_debug_\#{ind}_\#{#{$prefix}_escape(var)}"
end
def #{$prefix}_escape(var)
  result = ''
  var.to_s.each_char do |c|
    if not c == '\n'
      result += c
    end
  end
  result
end
def move
  puts "\n#{$prefix}_move"
end
def turn(dir = :back)
  case dir
    when :right then puts "\n#{$prefix}_turn_right"
    when :left then puts "\n#{$prefix}_turn_left"
    when :back then puts "\n#{$prefix}_turn_back"
    else raise(ArgumentError, "unknown argument")
  end
end
def put(elem = :buoy)
  case elem
    when :buoy then puts "\n#{$prefix}_put_buoy"
    when :treasure then puts "\n#{$prefix}_put_treasure"
    else raise(ArgumentError, "unknown argument")
  end
end
def take
  puts "\n#{$prefix}_take"
end
def break_point(dir = :point)
  case dir
    when :point then puts "\n#{$prefix}_break_point"
    when :up then puts "\n#{$prefix}_break_up"
    when :down then puts "\n#{$prefix}_break_down"
  end
end
def #{$prefix}_line(i)
  puts "\n#{$prefix}_line_\#{i}"
end
def look(dir = :here)
  case dir
    when :right then puts "\n#{$prefix}_?_look_right"
    when :left then puts "\n#{$prefix}_?_look_left"
    when :here then puts "\n#{$prefix}_?_look_here"
    when :back then puts "\n#{$prefix}_?_look_back"
    when :front then puts "\n#{$prefix}_?_look_front"
    else raise(ArgumentError, "unknown argument")
  end
  ret = gets.chomp
  case ret
    when 'buoy' then return :buoy
    when 'monster' then return :monster
    when 'treasure' then return :treasure
    when 'wave' then return :wave
    when 'border' then return :border
    else return :nothing
  end
end
]
  end

end
