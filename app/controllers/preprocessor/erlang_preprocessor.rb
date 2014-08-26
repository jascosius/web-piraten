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

  # Processes the given code...
  def process_code(code_msg, vars)
    codes = ''
    i = 1
    code_msg.each_line do |line|
      ind_arrow = line.index('->')
      if ind_arrow
        ind_arrow += 2
        line = line.insert(ind_arrow, " a#{$prefix}_line(#{i}),")
      end
      codes += line.chomp + "  % #{$prefix}_(#{i}#{$prefix}_)\n"
      i += 1
    end
    insert_start_logic + codes
  end

  def postprocess_print(send, type, line)
    if type == 'compile'
      if @compileflag
        @compileflag = false
        send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
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
                                            "~s:~p: error: bad function ~p~n  in function ~p:~p/~p~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))),              % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace)))),              % Line
                                             Fun,
                                             element(1,Message), element(2,Message),element(3,Message)]); % Module:Function/Arity
                error:{badarity, Fun}   -> Trace = erlang:get_stacktrace(),
                                           io:fwrite(standard_error,
                                            "~s:~p: error: bad arity~n  interpreted function called with number of arguments unequal to its arity~n",
                                            [element(2,lists:nth(1,element(4,lists:nth(1,Trace)))),   % File
                                             element(2,lists:nth(2,element(4,lists:nth(1,Trace))))]); % Line
                ExceptionClass:Term     -> io:fwrite(standard_error, "~p: ~p", [ExceptionClass, Term])
                                           % standard error handling for all other exceptions
              end, halt().

    aCkyUHZVL3q_line(I) -> io:fwrite("~nCkyUHZVL3q_line_~p~n", [I]).

    move()  -> io:fwrite("~nCkyUHZVL3q_move~n").

    take()  -> io:fwrite("~nCkyUHZVL3q_take~n").

    puts()         -> puts(buoy).
    puts(buoy)     -> io:fwrite("~nCkyUHZVL3q_put_buoy");
    puts(treasure) -> io:fwrite("CkyUHZVL3q_put_treasure").

    turn()      -> turn(back).
    turn(back)  -> io:fwrite("~nCkyUHZVL3q_turn_back~n");
    turn(right) -> io:fwrite("~nCkyUHZVL3q_turn_right~n");
    turn(left)  -> io:fwrite("~nCkyUHZVL3q_turn_left~n").

    look()      -> look(here).
    look(here)  -> io:fwrite("~nCkyUHZVL3q_?_look_here~n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(front) -> io:fwrite("~nCkyUHZVL3q_?_look_front~n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(left)  -> io:fwrite("~nCkyUHZVL3q_?_look_left~n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(right) -> io:fwrite("~nCkyUHZVL3q_?_look_right~n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(back)  -> io:fwrite("~nCkyUHZVL3q_?_look_back~n"),
                   lists:nth(1,element(2,io:fread("", "~a"))).

]
  end
end
