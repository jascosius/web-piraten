# -*- encoding : utf-8 -*-
class ErlangPreprocessor < BasePreprocessor

  attr :filename
  attr :compile
  attr :execute
  attr :compile_error
  attr :execute_error
  attr :line_first

  def initialize(attribut)
    super(attribut)
    @line_first = true
  end

  def commands_for_vm(code, tracing_vars)
    [{:write_file => {:filename => 'webpiraten.erl', :content => process_code(code, tracing_vars)}},
     {:execute => {:command => 'erlc -W0 webpiraten.erl', :stderr => 'compile', :stdout => 'compile'}},
     {:execute => {:command => 'erl -noshell -s webpiraten main -s init stop'}},
     {:exit => {}}]
  end

  def process_code(code_msg, vars)
    codes = ''
    i = 1
    highlighting = false
    code_msg.each_line do |line|
      puts "line number #{i}"
      open = 0
      arrows = line.scan('->')
      stops = line.scan(/(?:.|end)/)
      if true
        ind_arrow = line.index('->')
        ind_comma = line.index(',')
        ind_end = line.index('end')
        ind_fullstop = line.index('.')
        index_sem = line.rindex(';')
        index_curlo = line.index('{')
        index_curlc = line.index('}')
      end
      if ind_arrow
        ind_arrow += 2
        line = line.insert(ind_arrow, " a#{$prefix}_line(#{i}),")
        highlighting = true
        open += 1
        #elsif !highlighting
        # maybe unnecessary, depends on further implementation
        # elsif ind_comma #&& highlighting
        #    if ind_end
        #      if ind_end < ind_comma
        #        ind_end += 3
        #        line = line.insert(ind_end, ", a#{$prefix}_line(#{i})")
        #      else
        #        line = line.insert(0, "a#{$prefix}_line(#{i}),")
        #        # maybe it's smarter but uglier in animation to insert before comma
        #      end
        #    else
        #      line = line.insert(ind_comma, ", a#{$prefix}_line(#{i})")
        #    end
        # elsif ind_fullstop
        #   line = line.insert(0, ", a#{$prefix}_line(#{i})")
      end
=begin      if index_end
        line = line.insert(index_end, ", a#{$prefix}_line(#{i})")
      elsif index_bra > index_com
        index_bra += 1
        if found_case
        line = line.insert(index_bra, ", a#{$prefix}_line(#{i}), ")
        else
          line = line.insert(index_bra, ", a#{$prefix}_line(#{i})")
        end
      elsif index_com
        line = line.insert(index_com, ", a#{$prefix}_line(#{i})")
      elsif index_sem
        line = line.insert(index_sem, ", a#{$prefix}_line(#{i})")
      elsif index_sta
        index_sta += 2
        line = line.insert(index_sta, " a#{$prefix}_line(#{i}),")
      end
=end
      codes += line.chomp + "  % #{$prefix}_(#{i}#{$prefix}_)\n"
      #codes += "#{$prefix}_line(#{i})\n" + line.chomp
      puts line
      i += 1
    end
    insert_start_logic + codes
  end

  def postprocess_print(_, type, line, code)
    if type == 'compile'
      postprocess_error_compile(line, code)
    elsif type == 'error'
      postprocess_error(line, code)
    end
  end

  def postprocess_error(line, code)
    puts "hier beginnt die line"
    puts line.to_s
    puts "hier endet die line"


    line
  end

  def postprocess_error_compile(line, code)
    #remove filepath
    index_begin = line.index(':') #filepath starts with /
    index_end = line.index(':', index_begin+1) if index_begin #filepath ends with filename
    if index_begin and index_end and index_begin < index_end #found a filepath?
      #index_end += "#{@filename}".length #add the length of the filename to the end
      #line.slice!(index_begin...index_end) #remove the filepath

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
        line = line.insert(index_begin+1, new_line)
        line = line.gsub(@filename, 'line')
        #line = line.insert(index_begin+1, new_line) #add the new linenumber to the error
        #line = line.insert(index_begin, 'line') #add a line to the error instead of the filepath
      end
    end


#     puts "postprocess compile line beginn"
#     puts line
#     puts "ppc ende"
#     index_begin = line.index(':')
#     puts index_begin
#     index_end = line.index(':', index_begin+1) if index_begin
#     puts index_end
#
#     if index_begin && index_end && index_begin < index_end
#       number = line[index_begin+1...index_end]
#       counter = number.to_i
# #      line.gsub!("#{@filename}", 'line')
#       puts number
#       i = 1
#       new_line = ''
# =begin
#       code.each_line do |li|
#         unless li.include?("#{$prefix}_EOF")
#           counter -= 1
#           puts "#{counter.to_s} in code: #{li}"
#         end
#       end
#       line.slice!(0, index_end)
#
#       if counter == 0 #new_line == ''
#         line.slice!(0..1)
#       else
#         line = line.insert(0, "line:#{counter.to_s}")
#       end
# =end
#       code.each_line do |li|
#         if i == number.to_i
#           line_begin = li.index("#{$prefix}_(")
#           line_end = li.index("#{$prefix}_)")
#           if line_begin && line_end
#             new_line = li[line_begin+"#{$prefix}_(".length...line_end]
#           end
#         end
#         i += 1
#       end
#       line.slice!(0, index_end)
#       if new_line == ''
#         line.slice!(index_begin..index_begin+1)
#       else
#         line = line.insert(0, "line:#{new_line}")
#       end
#     end
#     puts "postprocesscompile"
#     puts line
#     puts "ende"

    line
  end

  def insert_start_logic
    %Q[
    -module(webpiraten).
    -export([main/0]).

    main() -> start(), halt().

    a#{$prefix}_line(I) -> io:fwrite("\n#{$prefix}_line_" ++ integer_to_list(I) ++ "\n").

    move()  -> io:fwrite("\n#{$prefix}_move\n").

    take()  -> io:fwrite("\n#{$prefix}_take\n").

    puts()         -> puts(buoy).
    puts(buoy)     -> io:fwrite('\n#{$prefix}_put_buoy\n');
    puts(treasure) -> io:fwrite('\n#{$prefix}_put_treasure\n');
    puts(_)        -> erlang:error(function_clause).

    turn()      -> turn(back).
    turn(back)  -> io:fwrite("\n#{$prefix}_turn_back\n");
    turn(right) -> io:fwrite("\n#{$prefix}_turn_right\n");
    turn(left)  -> io:fwrite("\n#{$prefix}_turn_left\n");
    turn(_)     -> erlang:error(function_clause).

    look()      -> look(here).
    look(here)  -> io:fwrite("\n#{$prefix}_?_look_here\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(front) -> io:fwrite("\n#{$prefix}_?_look_front\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(left)  -> io:fwrite("\n#{$prefix}_?_look_left\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(right) -> io:fwrite("\n#{$prefix}_?_look_right\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(back)  -> io:fwrite("\n#{$prefix}_?_look_back\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(_)     -> erlang:error(function_clause).
]
  end
end
