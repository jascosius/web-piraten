# -*- encoding : utf-8 -*-
class ErlangPreprocessor

  attr :line_first

  def initialize(code,tracing_vars)
    @line_first = true
    @compileflag = true
    @process_code = process_code(code, tracing_vars)
  end

  def commands_for_vm
    [{:write_file => {:filename => 'webpiraten.erl', :content => @process_code}},
     {:execute => {:command => 'erlc -W0 webpiraten.erl', :stderr => 'compile', :stdout => 'compile', :permissions => 'read-write'}},
     {:execute => {:command => 'echo ok', :stdout => 'ok'}}]
  end

  def regex_verify_string_comment
    /('(?:[^']|(?:\\'))*%(?:[^']|(?:\\'))*'|"(?:[^"]|(?:\\"))*%(?:[^"]|(?:\\"))*")/
  end

  def is_not_in_string?(index, string_indexes)
    not_in_string = true
    string_indexes.each do |pair|
      if index > pair[:starts] && index <= pair[:ends]
        not_in_string = false
      end
    end
    not_in_string
  end

  def scan_for_index_start_n_end(code, regex)
    res = []
    code.scan(regex) do
      res << {starts: Regexp.last_match.offset(0).first, ends: Regexp.last_match.offset(0).last}
    end
    res
  end

  def remove_comments(code_msg)
    codes = ''
    code_msg.each_line do |line|
      res_c = []
      line.scan('%') do
        res_c << Regexp.last_match.offset(0).first
      end
      res_s = scan_for_index_start_n_end(line, regex_verify_string_comment)
      if !res_c.empty? && !res_s.empty?
        res_c.each do |res|
          if is_not_in_string?(res, res_s)
            line.slice!(res..-1)
          end
        end
      elsif !res_c.empty?
        line.slice!(res_c[0]..-1)
      end
      codes += line.chomp + "\n"
    end
    codes
  end

  def regex_find_strings
    /(?:'(?:[^']|(?:\\'))*'|"(?:[^"]|(?:\\"))*")/
  end

  def regex_find_operations
    /(?:\bmove\(|\btake\(|\blook\(|\bputs\(|\bturn\(|->|\.)/
  end

  def regex_arrow_prefix
    Regexp.new("->line#{$prefix}")
  end

  def regex_semicolon_prefix
    Regexp.new(";line#{$prefix}")
  end

  def regex_end_prefix
    Regexp.new("endline#{$prefix}")
  end

  def regex_stop_prefix
    Regexp.new("\\.line#{$prefix}")
  end

  def regex_op_prefix
    Regexp.new("\\(line#{$prefix}")
  end

  def insert_prefix(code, operations, strings)
    if strings.empty?
      operations.reverse_each do |op|
        code.insert(op[:ends], "line#{$prefix}")
      end
    else
      operations.reverse_each do |op|
        code.insert(op[:ends], "line#{$prefix}") if is_not_in_string?(op[:starts], strings)
      end
    end
    code
  end

  def change_prefix_2_line_number(code)
    new_code = ''
    number = 1
    first_arrow = true
    code.each_line do |line|
      if first_arrow
        first_arrow = false if line.gsub!(regex_arrow_prefix, "-> a#{$prefix}_line(#{number}), a#{$prefix}_break(fun() ->")
      else
        line.gsub!(regex_arrow_prefix, "-> a#{$prefix}_line(#{number}), ")
      end
      first_arrow = true if line.gsub!(regex_stop_prefix, ' end).')
      line.gsub!(regex_op_prefix, "(#{number}, ")
      line.gsub!(/\(\d+,\s+\)/, "(#{number})")
      new_code += line.chomp + "  % #{$prefix}_(#{number}#{$prefix}_)\n"
      number += 1
    end
    new_code
  end

  # Processes the given code...
  def process_code(code_msg, vars)
    new_code = remove_comments(code_msg)
    string_indexes = scan_for_index_start_n_end(new_code, regex_find_strings)
    operation_indexes = scan_for_index_start_n_end(new_code, regex_find_operations)
    if string_indexes.empty? && !operation_indexes.empty?
      new_code = insert_prefix(new_code, operation_indexes, [])
      new_code = change_prefix_2_line_number(new_code)
    elsif !string_indexes.empty? && !operation_indexes.empty?
      new_code = insert_prefix(new_code, operation_indexes, string_indexes)
      new_code = change_prefix_2_line_number(new_code)
    end
    insert_start_logic + new_code
  end


  def postprocess_print(send, type, line)
    if type == 'compile'
      if @compileflag
        @compileflag = false
        send.call([{:exit => {:succsessful => false, :message => 'Syntaxfehler'}}])
      end
      postprocess_error_compile(line)
    elsif type == 'ok' and @compileflag
      send.call([{:execute => {:command => 'erl -noshell -s webpiraten main -s init stop'}}, {:exit => {}}])
      {:type => :no}
    elsif type == 'error'
      postprocess_error(line)
    else
      {:type => :no}
    end
  end

  # Processes a given error message, when the execution is aborted with an error.
  # If there are information about line numbers in the error message the method
  # will check if there is a corresponding line number which is visible to the
  # user.
  def postprocess_error(line)
    if line =~ /webpiraten\.erl:\d*:/ # process message only if there's the filename
      index_begin = line.index(':') # find line number beginning
      index_end = line.index(':', index_begin+1) # find line number ending
      if index_begin && index_end && index_begin < index_end
        line_number = line[index_begin+1, index_end].to_i # extract error line number
        i = 1
        new_line_number = ''
        @process_code.each_line do |lin|
          if i == line_number # find the line from the error message
            line_begin = lin.index("#{$prefix}_(") # find the begin of the original line number in comment
            line_end = lin.index("#{$prefix}_)") # find the end of the original line number in comment
            if line_begin and line_end # found something?
              new_line_number = lin[line_begin+"#{$prefix}_(".length...line_end]
              # set the new line number to the number in the comment
            end
          end
          i += 1
        end

        line.slice!(index_begin+1...index_end) #remove the old line number from the error

        if new_line_number == '' #is there a result for the new line number?
          line.slice!(0..index_begin+2) # remove erverything up to colon after the old number
        else
          # add the new line number to the error and exchange filename with 'line'
          line = line.insert(index_begin+1, new_line_number)
          line = line.insert(index_begin, 'line')
          line.slice!(0...index_begin)
        end
        # remove specific module information in the error message for the used module 'webpiraten'
        line.slice!('webpiraten:')
      end
    end
    {:type => :error, :message => line}
  end

  # Processes a given error message, when the compiling ended with an error.
  # The handling and functionality is similar to postprocess_error.
  def postprocess_error_compile(line)
    if line =~ /webpiraten\.erl:\d*:/ # process message only if there's the filename
      index_begin = line.index(':') # find line number beginning
      index_end = line.index(':', index_begin+1) # find line number ending
      if index_begin && index_end && index_begin < index_end
        line_number = line[index_begin+1, index_end].to_i # extract error line number
        i = 1
        new_line_number = ''
        @process_code.each_line do |lin|
          if i == line_number # find the line from the error message
            line_begin = lin.index("#{$prefix}_(") # find the begin of the original line number in comment
            line_end = lin.index("#{$prefix}_)") # find the end of the original line number in comment
            if line_begin and line_end # found something?
              new_line_number = lin[line_begin+"#{$prefix}_(".length...line_end]
              # set the new line number to the number in the comment
            end
          end
          i += 1
        end

        line.slice!(index_begin+1...index_end) #remove the old line number from the error

        if new_line_number == '' #is there a result for the new line number?
          line.slice!(0..index_begin+2) # remove erverything up to colon after the old number
        else
          # add the new line number to the error and exchange filename with 'line'
          line = line.insert(index_begin+1, new_line_number)
          line = line.insert(index_begin, 'line')
          line.slice!(0...index_begin)
        end
        # remove specific module information in the error message for the used module 'webpiraten'
        line.slice!('webpiraten:')
      end
    end
    {:type => :error, :message => line}
  end

  # String that contains logic for the pirateships movements and also preprocesses
  # raised error messages in a pretty and easy understandable way.
  def insert_start_logic
    %Q[
    -module(webpiraten).
    -export([main/0]).

    main() -> try
                start()
              catch
                error:function_clause   -> Trace = erlang:get_stacktrace(),
                                           io:fwrite(standard_error,
                                            "~s:~p: error: no function clause matching ~p:~p(~p)~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(2,Trace)))), % File
                                             element(2,lists:nth(2,element(4,lists:nth(2,Trace)))), % Line
                                             element(1,lists:nth(1,Trace)),                         % Module
                                             element(2,lists:nth(1,Trace)),                         % Function
                                             lists:nth(1,element(3,lists:nth(1,Trace)))]);          % Value
                error:{case_clause,Val} -> Trace = erlang:get_stacktrace(),
                                           io:fwrite(standard_error,
                                            "~s:~p: error: no case clause matching ~p~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))), % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace)))), % Line
                                             Val]);
                error:if_clause         -> Trace = erlang:get_stacktrace(),
                                           io:fwrite(standard_error,
                                            "~s:~p: error: no true branch found when evaluating an if expression~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))),    % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace))))]); % Line
                error:{badmatch,Val}    -> Trace = erlang:get_stacktrace(),
                                           io:fwrite(standard_error,
                                            "~s:~p: error: no match of right hand side value ~p~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))), % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace)))), % Line
                                             Val]);
                error:badarg            -> Trace = erlang:get_stacktrace(),
                                           % unfortunately unable to extract function and arity
                                           % io:fwrite("~s:~p: error: bad argument~n  in function ~p/~p ~n  called as ~p(~p)~n",
                                           % [File, Line, Function, Arity, Function, Argument])
                                           io:fwrite(standard_error, "~s:~p: error: bad argument~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))), % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace))))]); % Line
                error:undef             -> Trace = erlang:get_stacktrace(),
                                           Message = lists:nth(1,Trace),
                                           % unable to extract position of call
                                           io:fwrite(standard_error, "error: undefined function ~p:~p/~p~n",
                                           [element(1,Message), element(2,Message),length(element(3,Message))]); % Module:Function/Arity
                error:badarith          -> Trace = erlang:get_stacktrace(),
                                           io:fwrite(standard_error,
                                            "~s:~p: error: bad argument in an arithmetic expression~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))),   % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace))))]); % Line
                error:{badfun, Fun}     -> Trace = erlang:get_stacktrace(),
                                           Message = lists:nth(1,Trace),
                                           io:fwrite(standard_error,
                                            "~s:~p: error: bad function ~p~n  in function ~p:~p~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))),              % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace)))),              % Line
                                             Fun,
                                             element(1,Message), element(2,Message)]); % Module:Function/Arity
                error:{badarity, Fun}   -> Trace = erlang:get_stacktrace(),
                                           io:fwrite(standard_error,
                                            "~s:~p: error: bad arity~n  interpreted function called with number of arguments unequal to its arity~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))),   % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace))))]); % Line
                ExceptionClass:Term     -> io:fwrite(standard_error, "~p: ~p", [ExceptionClass, Term])
                                           % standard error handling for all other exceptions
              end, halt().

    a#{$prefix}_line(I) -> io:fwrite("~n#{$prefix}_line_~p~n", [I]).

    a#{$prefix}_break(F) -> a#{$prefix}_break_point(down),
                            X = F(),
                            a#{$prefix}_break_point(up),
                            X.

    a#{$prefix}_break_point(up)   -> io:fwrite("~n#{$prefix}_break_up~n");
    a#{$prefix}_break_point(down) -> io:fwrite("~n#{$prefix}_break_down~n").

    move(I)  -> a#{$prefix}_line(I),
                io:fwrite("~n#{$prefix}_move~n").

    take(I)  -> a#{$prefix}_line(I),
                io:fwrite("~n#{$prefix}_take~n").

    puts(I)           -> puts(I, buoy).
    puts(I, buoy)     -> a#{$prefix}_line(I),
                         io:fwrite("~n#{$prefix}_put_buoy~n");
    puts(I, treasure) -> a#{$prefix}_line(I),
                         io:fwrite("~n#{$prefix}_put_treasure~n").

    turn(I)        -> turn(I, back).
    turn(I, back)  -> a#{$prefix}_line(I),
                      io:fwrite("~n#{$prefix}_turn_back~n");
    turn(I, right) -> a#{$prefix}_line(I),
                      io:fwrite("~n#{$prefix}_turn_right~n");
    turn(I, left)  -> a#{$prefix}_line(I),
                      io:fwrite("~n#{$prefix}_turn_left~n").

    look(I)        -> look(I, here).
    look(I, here)  -> a#{$prefix}_line(I),
                      io:fwrite("~n#{$prefix}_?_look_here~n"),
                      lists:nth(1,element(2,io:fread("", "~a")));
    look(I, front) -> a#{$prefix}_line(I),
                      io:fwrite("~n#{$prefix}_?_look_front~n"),
                      lists:nth(1,element(2,io:fread("", "~a")));
    look(I, left)  -> a#{$prefix}_line(I),
                      io:fwrite("~n#{$prefix}_?_look_left~n"),
                      lists:nth(1,element(2,io:fread("", "~a")));
    look(I, right) -> a#{$prefix}_line(I),
                      io:fwrite("~n#{$prefix}_?_look_right~n"),
                      lists:nth(1,element(2,io:fread("", "~a")));
    look(I, back)  -> a#{$prefix}_line(I),
                      io:fwrite("~n#{$prefix}_?_look_back~n"),
                      lists:nth(1,element(2,io:fread("", "~a"))).
]
  end
end
