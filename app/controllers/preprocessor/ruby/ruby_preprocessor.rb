# -*- encoding : utf-8 -*-
class RubyPreprocessor

  require 'preprocessor/ruby/processing_tools'

  attr :line_first

  def initialize(code, tracing_vars)
    @operationlist = []
    @end_break = ''
    @beg_break = ''
    @line_first = true
    @filename = "#{$prefix}_code.rb"
    @syntaxflag = true
    @code = code
    @tracing_vars = tracing_vars
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
    performance = Time.now
    i=1
    codes = ''
    bools = {}
    bools[:multiline_comment] = false
    bools[:single_q] = false
    bools[:double_q] = false
    bools[:dont_skip_line] = true
    bools[:string_at_beginning] = false
    if code_msg =~ regex_verify_case_statement
      bools[:no_multiline_case_statement] = true
      bools[:multiline_string] = false
      bools[:found_case] = false
      bools[:found_when] = false
      code_msg.each_line do |s|
        codes += insert_line_number(i, bools[:dont_skip_line])
        bools = case_block_processing(s, bools)
        codes += add_user_codeline(s, i, bools)
        codes = insert_debug_information(codes, vars, bools[:dont_skip_line])
        i += 1
      end
    elsif code_msg =~ regex_verify_multiline_string
      code_msg.each_line do |s|
        codes += insert_line_number(i, bools[:dont_skip_line])
        bools = multiline_processing(s, bools)
        codes += add_user_codeline(s, i, bools)
        codes = insert_debug_information(codes, vars, bools[:dont_skip_line])
        i += 1
      end
    else
      code_msg.each_line do |s|
        codes += insert_line_number(i, true)
        codes += add_user_codeline(s, i, bools)
        codes = insert_debug_information(codes, vars, true)
        i += 1
      end
    end
    puts insert_logic + codes + "\n"
    result = insert_logic + codes + "\n"

    PERFORMANCE_LOGGER.store :preprocessor, performance, Time.now
    return result
  end

  # Method to add the linenumber for linehighlighting in codemirror. Previous
  # to the user's line of code.
  def insert_line_number(i, dont_skip_line)
    dont_skip_line ? "#{$prefix}_line(#{i})\n" : ''
  end

  # Adds the line of the user's code and a commment with the linenumber, doesn't add
  # a comment if it's processing a multiline string.
  def add_user_codeline(s, i, bools)
    code = s.chomp
    s = s.chomp
    #multiline comments
    if code.start_with?('=end')
      bools[:multiline_comment] = false
      s = s[4..-1]
    elsif ((bools[:multiline_comment] == true) || (code.start_with?('=begin')))
      bools[:multiline_comment] = true
      return code + " # #{$prefix}_(#{i}#{$prefix}_)\n"
    end
    code = modify_code(s,bools,code)         #[modified string, position of comment] find also ends and returns
    #add code
    if bools[:dont_skip_line]
      code = code + @end_break + " # #{$prefix}_(#{i}#{$prefix}_)\n"
      @end_break = ''
    end
    if bools[:string_at_beginning] == false
      code = @beg_break + code
    end
    #strings at the beginning of the next line
    if !bools[:dont_skip_line]
      if bools[:single_q]
        bools[:string_at_beginning] = :sq
      end
      if bools[:double_q]
        bools[:string_at_beginning] = :dq
      end
    else
      bools[:string_at_beginning] = false
    end
    @beg_break = ''
    code
  end

  def modify_code(s,bools,code)
    #multiline string at the beginning
    if bools[:string_at_beginning] == :dq
      if  s =~ /"/
        s = s[(s.index(/"/) + 1)..-1]
      else
        s = ''
      end
    elsif bools[:string_at_beginning] == :sq
      if  s =~ /'/
        s = s[(s.index(/'/) + 1)..-1]
      else
        s = ''
      end
    end
    finding_loop(s,code)
  end

  #loop for modifying the control code by finding strings and comments
  def finding_loop(s,code)
    performance = Time.now
    is_while = false
    expressions = [[/"/, :dq],
                   [/\breturn\b/, :return],
                   [/\bend\b/,:end],
                   [/'/, :sq],
                   [/#/, :com],
                   [/\bdo\b/, :do],
                   [/(?:\bdef\b|\bclass\b|\bmodule\b|\bEND\b|\bBEGIN\b|\bbegin\b)/, :def],
                   [/(?:\buntil\b|\bwhile\b|\bif\b|\bcase\b)/, :while]]
    loop do
      position = [nil, s.length]
      #find first symbol
      expressions.length.times do |i|
        this_pos = s.index(expressions[i][0])
        if this_pos != nil && this_pos < position[1]
          position = [expressions[i][1], this_pos]
        end
      end
      # next of the same symbol
      case position[0]
        when :dq, :sq
          exp1 = /'/
          exp2 = /[^\\]'/
          if position[0] == :dq
            exp1 = /"/
            exp2 =/[^\\]"/
          end
          next_curr1 = s.index(exp1,position[1]+1)
          next_curr2 = s.index(exp2,position[1]+1)
          if  next_curr1 == position[1]+1
            s = s[0..position[1]-1] + s[next_curr1+1..-1]
          elsif next_curr2 != nil
            s = s[0..position[1]-1] + s[next_curr2+2..-1]
          else
            PERFORMANCE_LOGGER.store :finding_loop, performance, Time.now
            return code
          end
        when :com
          diff = code.length - s.length
          code.insert(position[1] + diff, @end_break)
          @end_break = ''

          PERFORMANCE_LOGGER.store :finding_loop, performance, Time.now
          return code
        when :return
          insert_code = return_break()
          diff = code.length - s.length
          code.insert(s.index(/\breturn\b/)+diff, insert_code)
          s = s[position[1]+5..-1]
        when :end
          insert_code = '; break_point(:up); '
          diff = code.length - s.length
          op = @operationlist.shift
            if op == :def
              code.insert(s.index(/\bend\b/)+diff, @end_break + insert_code)
              @end_break = ''
            elsif op != nil
              if code.length <= s.index(/\bend\b/)+diff+4
                code = code + insert_code
              else
                code.insert(s.index(/\bend\b/)+diff+4, insert_code)
                code.insert(s.index(/\bend\b/)+diff, @end_break)
                @end_break = ''
              end
            end
          s = s[position[1]+3..-1]
        when :do
          if is_while
            s = s[position[1]+2..-1]
          else
            @operationlist.unshift(:do)
            @beg_break = 'break_point(:down); ' + @beg_break
            s = s[position[1]+2..-1]
          end
        when :while
          @operationlist.unshift(:do)
          @beg_break = 'break_point(:down); ' + @beg_break
          s = s[position[1]+3..-1]
          is_while = true
        when :def
          @operationlist.unshift(:def)
          @end_break = @end_break + '; break_point(:down)'
          s = s[position[1]+3..-1]
        else
          PERFORMANCE_LOGGER.store :finding_loop, performance, Time.now
          return code
      end
    end
  end

  def return_break()
    operationlist = Array.new(@operationlist)
    op = operationlist.shift
    beg_break = '; '
    loop do
      beg_break =  beg_break + 'break_point(:up); '
      if operationlist.empty? ||  op == :def
        break
      end
      op = operationlist.shift
    end
    return beg_break
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
    performance = Time.now
    if type == 'checksuccess'
      @process_code = process_code(@code, @tracing_vars)
      send.call([{:write_file => {:filename => @filename, :content => @process_code}}, {:execute => {:command => "ruby #{@filename}"}}, {:exit => {}}])
      result = {:type => :no}
      PERFORMANCE_LOGGER.store :postprocess_print, performance, Time.now
      return result
    elsif type == 'checkerror'
      if @syntaxflag
        send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
        @syntaxflag = false
      end
      result = {:type => :error, :message => line}

      PERFORMANCE_LOGGER.store :postprocess_print, performance, Time.now
      return result
    else
      result = postprocess_execute(line)
      PERFORMANCE_LOGGER.store :postprocess_print, performance, Time.now
      return result
    end
  end

  def postprocess_execute(line)
    performance = Time.now
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
    result = {:type => :error, :message => line}
    PERFORMANCE_LOGGER.store :postprocess_execute, performance, Time.now
    return result
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
def turn(direction = :back)
  if [:right, :left, :back].include? direction
    puts "\n#{$prefix}_turn_\#{direction}"
  else
    raise(ArgumentError, "unknown argument")
  end
end
def put(element = :buoy)
  if [:buoy, :treasure].include? element
    puts "\n#{$prefix}_put_\#{element}"
  else
    raise(ArgumentError, "unknown argument")
  end
end
def take
  puts "\n#{$prefix}_take"
end
def break_point(direction = :point)
  if [:point, :up, :down].include? direction
    puts "\n#{$prefix}_break_\#{direction}"
  else
    raise(ArgumentError, "unknown argument")
  end
end
def #{$prefix}_line(i)
  puts "\n#{$prefix}_line_\#{i}"
end
def look(direction = :here)
  if [:right, :left, :here, :back, :front].include? direction
    puts "\n#{$prefix}_?_look_\#{direction}"
  else
    raise(ArgumentError, "unknown argument")
  end
  gets.chomp.to_sym
end
]
  end

end
