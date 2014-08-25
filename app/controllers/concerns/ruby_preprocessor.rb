# -*- encoding : utf-8 -*-
class RubyPreprocessor < BasePreprocessor

  attr :filename
  attr :compile
  attr :execute
  attr :compile_error
  attr :execute_error

  attr :line_first

  def initialize(attribut)
    super(attribut)
    @filename = "#{$prefix}_code.rb"
    @compile = ''
    @execute = "ruby $PATH$/#{$prefix}_code.rb" #$PATH$ will be replaced
    @compile_error = ''
    @execute_error = ''
    @operationlist = []
    @end_break = ''
    @beg_break = ''
    @line_first = true
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
    bools[:string_at_beginning] = false
    if code_msg =~ regex_verify_case_statement
      codes = case_block_processsing(code_msg, vars, i, '', bools)
    elsif code_msg =~ /(?:'(?:[^']?(?:\\')?)*(?:\n|\r|\r\n)(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*(?:\n|\r|\r\n)(?:[^"]?(?:\\")?)*")/
      # Regular expression that verifies the existence of a multiline string in
      # the code.
      #codes = multiline_processing(code_msg, vars, i, '')
      code_msg.each_line do |s|
        codes += insert_line_number(i, bools[:dont_skip_line])
        bools = multiline_processing(s, bools)
        codes += add_user_codeline(s, i, bools)
        codes = insert_debug_information(codes, vars, bools[:dont_skip_line])
        i += 1
      end
    else
      codes = ''
      code_msg.each_line do |s|
        codes += insert_line_number(i, true)
        codes += add_user_codeline(s, i, bools)
        codes = insert_debug_information(codes, vars, true)
        i += 1
      end
    end
    insert_logic + codes + "\n"
  end

  # Method that processes the code, if there is a case block in the user's code.
  # The code is processed line by line and the further insertion of information is
  # controlled by booleans and verifying regular expressions.
  # Still missing: multiline strings outside the case block and multiline when
  # statements.
  def case_block_processsing(code_msg, vars, i, codes, bools)
    no_multiline_case_statement = true
    multiline_string = false
    found_case = false
    found_when = false
    single_q = false
    double_q = false
    code_msg.each_line do |s|
      codes += insert_line_number(i, bools[:dont_skip_line])
      if multiline_string # processes a found multiline string
        bools = multiline_processing(s, bools)
        if bools[:dont_skip_line] && s =~ regex_verify_when_after_multiline
          multiline_string = false
          found_case = true
          found_when = true
          bools[:dont_skip_line] = false
          if s =~ /when\s*(?:#+.*)?$/
          elsif s =~ regex_verify_when_sq
            single_q = true
          elsif s =~ regex_verify_when_dq
            double_q = true
          elsif s =~ regex_verify_when_complete
            found_case = false
            found_when = false
            bools[:dont_skip_line] = true
          end
        elsif bools[:dont_skip_line]
          multiline_string = false
        end
      elsif !found_case && !found_when # Standard setting for processing of
        # 'normal' code unless other flags are set.
        if s =~ regex_verify_case_when
          if s =~ regex_verify_when_sq
            single_q = true
            bools[:dont_skip_line] = false
            found_case = true
            found_when = true
          elsif s =~ regex_verify_when_dq
            double_q = true
            bools[:dont_skip_line] = false
            found_case = true
            found_when = true
          elsif s =~ regex_verify_when_complete

          else
            found_case = true
            found_when = true
            bools[:dont_skip_line] = false
          end
        elsif s =~ /;?\s*case\s*/
          bools[:dont_skip_line] = false
          found_case = true
          if s =~ /case\s*(?:'(?:[^']?(?:\\')?)*)$/ # verifies multiline string
            single_q = true
            no_multiline_case_statement = false
          elsif s =~ /case\s*(?:"(?:[^"]?(?:\\")?)*)$/ # verifies multiline string
            double_q = true
            no_multiline_case_statement = false
          end
        elsif s =~ /;?\s*\w*[!\?]*\s*=\s*(?:"(?:[^"]*(?:\\")?)*|'(?:[^']*(?:\\')?)*)$/
          # verifies multiline string
          bools = multiline_processing(s, bools)
          multiline_string = true
        elsif s =~ /'.*when.*'/
          # Prevents the word 'when' in single-line strings to be mistaken for a when
          # of a case statement.

        elsif s =~ /".*when.*"/

        elsif s =~ /when/
          found_case = true
          found_when = true
          bools[:dont_skip_line] = false
          if s =~ /when\s*(?:#+.*)?$/
          elsif s =~ regex_verify_when_sq
            single_q = true
          elsif s =~ regex_verify_when_dq
            double_q = true
          elsif s =~ regex_verify_when_complete
            found_case = false
            found_when = false
            bools[:dont_skip_line] = true
          end
        end
      elsif found_case && !found_when # beginning of a case statement has been found
        if !no_multiline_case_statement # found a multiline string for statement
          if single_q && s =~ /(?:[^']?(?:\\')?)*[^\\]?'/
            single_q = false
            no_multiline_case_statement = true
            if s =~ /when\s*(?:#+.*)?$/
              found_when = true
            elsif s =~ regex_verify_when_sq
              single_q = true
              found_when = true
            elsif s =~ regex_verify_when_dq
              double_q = true
              found_when = true
            elsif s =~ regex_verify_when_complete
              bools[:dont_skip_line] = true
              found_case = false
            end
          elsif double_q && s =~ /(?:[^"]?(?:\\")?)*[^\\]?"\s*/
            double_q = false
            no_multiline_case_statement = true
            if s =~ /when\s*(?:#+.*)?$/
              found_when = true
            elsif s=~ regex_verify_when_sq
              single_q = true
              found_when = true
            elsif s =~ regex_verify_when_dq
              double_q = true
              found_when = true
            elsif s =~ regex_verify_when_complete
              bools[:dont_skip_line] = true
              found_case = false
            end
          end
        elsif s =~ /\s*(?::*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")?\s*when/
          # found rest of case statement and the following when
          if s =~ /when\s*(?:#+.*)?$/
            found_when = true
          elsif s=~ regex_verify_when_sq
            single_q = true
            found_when = true
          elsif s =~ regex_verify_when_dq
            double_q = true
            found_when = true
          elsif s =~ regex_verify_when_complete
            bools[:dont_skip_line] = true
            found_case = false
          end
        elsif s =~ /(?:'(?:[^']?(?:\\')?)*)$/ # verifies beginning of case multiline string, single quoted
          single_q = true
          no_multiline_case_statement = false
        elsif s =~ /(?:"(?:[^"]?(?:\\")?)*)$/ # verifies beginning of case multiline string, double quoted
          double_q = true
          no_multiline_case_statement = false
        elsif s =~ /when/
          found_when = true
        end
      elsif found_case && found_when
        if single_q && s =~ /(?:[^']?(?:\\')?)*[^\\]?'\s*(?:#+.*)?$/
          # verifies end of multiline string, single quotes
          found_case = false
          found_when = false
          single_q = false
          bools[:dont_skip_line] = true
        elsif double_q && s =~ /(?:[^"]?(?:\\")?)*[^\\]?"\s*(?:#+.*)?$/
          # verifies end of multiline string, double quotes
          found_case = false
          found_when = false
          double_q = false
          bools[:dont_skip_line] = true
        elsif double_q || single_q

        elsif s=~ /(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*(?:#.*)?/
          # verifies end of first when statement
          found_case = false
          found_when = false
          bools[:dont_skip_line] = true
        elsif s =~ /(?:'(?:[^']?(?:\\')?)*)$/ # verifies multiline string, single quotes
          single_q = true
        elsif s =~ /(?:"(?:[^"]?(?:\\")?)*)$/ # verifies multiline string, double quotes
          double_q = true
        end
      else # Just in case something goes wrong process the code without inserting
        # any information.
        bools[:dont_skip_line] = false
      end
      codes += add_user_codeline(s, i, bools[:dont_skip_line])
      codes = insert_debug_information(codes, vars, bools[:dont_skip_line])
      i += 1
    end
    codes
  end

  # Processing of the code if there are multiline strings and no other structures
  # that need a special treatment. It's done through matching the given code lines
  # to specific patterns that determine the beginnings and ends of multiline
  # strings.
  def multiline_processing(s, booleans)
    single_q = booleans[:single_q]
    double_q = booleans[:double_q]
    dont_skip_line = booleans[:dont_skip_line]
    if single_q && !double_q
      if s =~ /^(?:[^']*(?:\\')?)*[^\\]?'\s*(?:;|\+|<<)/
        single_q = false
        if s =~ /"(?:[^"]*(?:\\")?)*$/
          double_q = true
        elsif s =~ /'(?:[^']*(?:\\')?)*$/
          single_q = true
        else
          dont_skip_line = true
        end
      elsif s=~ /^(?:[^']*(?:\\')?)*'\s*(?:#.*)?/
        single_q = false
        dont_skip_line = true
      end
    elsif double_q && !single_q
      if s=~ /^(?:[^"]*(?:\\")?)*[^\\]?"\s*(?:;|\+|<<)/
        double_q = false
        if s =~ /'(?:[^']*(?:\\')?)*$/
          single_q = true
        elsif s =~ /"(?:[^"]*(?:\\")?)*$/
          double_q = true
        else
          dont_skip_line = false
        end
      elsif s =~ /^(?:[^"]?(?:\\")?)*[^\\]?"\s*(?:#.*)?/
        double_q = false
        dont_skip_line = true
      end
    elsif s =~ /;?\s*\w*[!\?]*\s*=\s*"(?:[^"]*(?:\\")?)*$/
      double_q = true
      dont_skip_line = false
    elsif s =~ /;?\s*\w*[!\?]*\s*=\s*'(?:[^']*(?:\\')?)*$/
      single_q = true
      dont_skip_line = false
    end
    booleans[:single_q] = single_q
    booleans[:double_q] = double_q
    booleans[:dont_skip_line] = dont_skip_line
    return booleans
  end

  # Regularexpression for validating case-blocks in the code.
  def regex_verify_case_statement
    /(?:.*;)?\s*case\s*(?:#+.*\s*)*(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*(?:#+.*\s*)*\s*when\s*(?:#+.*\s*)*(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")/
  end

  # Verifies if the given line has a complete case statement from the case to
  # the first when.
  def regex_verify_case_when
    /case\s*(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*when/
  end

  # In case there is a when statement after a multiline string and to
  # prevent that there is a when inside the string mistaken as one of
  # a case statement.
  def regex_verify_when_after_multiline
    /(?:(?:[^']?(?:\\')?)*when(?:[^']?(?:\\')?)*[^\\]'|(?:[^"]?(?:\\")?)*when(?:[^"]?(?:\\")?)*[^\\]?"|(?:[^']?(?:\\')?)*[^\\]'|(?:[^"]?(?:\\")?)*[^\\]");?\s*when/
  end

  # Verifies start of multiline string with single quotation marks.
  def regex_verify_when_sq
    /when\s*(?:'(?:[^']?(?:\\')?)*)$/
  end

  # Verifies start of multiline string with double quotation marks.
  def regex_verify_when_dq
    /when\s*(?:"(?:[^"]?(?:\\")?)*)$/
  end

  def regex_verify_when_complete
    /when\s*(?:\s:*\w+\s|'(?:[^']?(?:\\')?)*'|"(?:[^"]?(?:\\")?)*")\s*(?:#.*)?$/
  end

  # Method to add the linenumber for linehighlighting in codemirror. Previous
  # to the user's line of code.
  def insert_line_number(i, dont_skip_line)
    dont_skip_line ? "#{$prefix}_line(#{i})\n" : ''
  end

  # Adds the line of the user's code and a commment with the linenumber, doesn't add
  # a comment if it's processing a multiline string.
  def add_user_codeline(s, i, bools)
    @comment = false
    code = s.chomp
    #comments?
    if s =~ /#/
      puts 'hier ist ein Kommentar: ' + s
      @comment = s.index('#')
      s = s[0..@comment]
    end
    #open functions
    if s=~ /\bdef\b/ || s=~/\bclass\b/ || s=~/\bmodule\b/ || s=~/\bEND\b/ || s=~/\bBEGIN\b/ || s=~/\bbegin\b/ #def class
      @operationlist.unshift(:defClass)
      @end_break = @end_break + '; break_point(:down)'
    elsif s=~ /\buntil\b/ || s=~ /\bwhile\b/ || s=~ /\bif\b/ || s=~ /\bcase\b/ # while until if case
      @operationlist.unshift(:doWhileIf)
      @beg_break = 'break_point(:down); ' + @beg_break
    elsif s=~ /\bdo\b/   #do when no [while, until, if, case] exist because while [expr] [do]...
      @operationlist.unshift(:doWhileIf)
      @beg_break = 'break_point(:down); ' + @beg_break
    end
    #closing functions
    puts 'first'
    if !@operationlist.empty?
      puts 'strike'
      if s=~ /\breturn\b/ #end
        @beg_break = return_break(@beg_break)
        puts 'here'
        puts @beg_break
      elsif s=~ /\bend\b/ #end
        op = @operationlist.shift
        if op == :defClass
          @beg_break = 'break_point(:up); ' + @beg_break
        else
          @end_break = @end_break + '; break_point(:up)'
        end
      end
    end
    #add code
    if @comment == false && bools[:dont_skip_line]
      code = @beg_break + code + @end_break + " # #{$prefix}_(#{i}#{$prefix}_)\n"
    elsif @comment != false
      code.insert(@comment, @end_break)
      code = @beg_break + code + " # #{$prefix}_(#{i}#{$prefix}_)\n"
    end

    @beg_break = ''
    @end_break = ''
    code
=begin
    #multilining
    puts bools[:dont_skip_line]
    puts !bools[:string_at_beginning]
    if bools[:dont_skip_line] then #no multiline at the end
      if !bools[:string_at_beginning] then #no multiline at the beginning
        puts 'no multiline' + s
        code = @beg_break + s.chomp + @end_break + " # #{$prefix}_(#{i}#{$prefix}_)\n"
        bools[:string_at_beginning] = false
        @beg_break = ''
        @end_break = ''
        code
      else                         #multiline at the beginning but not at the end
        bools[:string_at_beginning] = false
        puts 'multiline at the beginning' + s
        puts s.index('"')
        puts s.index(/[']/)
        code = s.chomp + @end_break + " # #{$prefix}_(#{i}#{$prefix}_)\n"
        @end_break = ''
        code
      end
    else                       #multiline at the end

      if !bools[:string_at_beginning]  #no multiline at the beginning
        code = @beg_break + s.chomp + " # #{$prefix}_(#{i}#{$prefix}_)\n"
        bools[:string_at_beginning] = true #multiline at the beginning of the next line
        puts 'multiline at the end' + s
        @beg_break = ''
        code
      else                     #multiline at the beginning and the end
        bools[:string_at_beginning] = true #multiline at the beginning of the next line
        puts 'multiline at the beginning and the end' + s
        s
      end
    end
=end
  end

  def return_break(break_up)
    operationlist = Array.new(@operationlist)
    op = operationlist.shift
    loop do
      @beg_break = 'break_point(:up); ' + @beg_break
      puts 'return'
      if operationlist.empty? ||  op == :defClass
        break
      end
      op = operationlist.shift
    end
    return @beg_break
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