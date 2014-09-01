# -*- encoding : utf-8 -*-
class ErlangPreprocessor

  require 'preprocessor/erlang/processing_tools'

  attr :line_first

  def initialize(code, tracing_vars)
    @line_first = true
    @compileflag = true
    @precompileflag = true
    @process_code = process_code(code, tracing_vars)
    @precompile_code = process_code_for_precompile(code)
  end

  #compile without changing code first, just add logic
  #when it fails print error message
  #when it succeeds compile with changing code for line-highlighting and so on
  #when this fails start in backup-mode, because it's not the users failure
  #when this succeeds run erlang
  def commands_for_vm
    [{:write_file => {:filename => 'prewebpiraten.erl', :content => @precompile_code}},
     {:execute => {:command => 'erlc -W0 prewebpiraten.erl', :stderr => 'precompile', :stdout => 'precompile', :permissions => 'read-write'}},
     {:execute => {:command => 'echo ok', :stdout => 'precompileok'}}]
  end

  # Processes the given code...
  def process_code(code_msg, vars)
    new_code = remove_comments(code_msg)
    new_code = insert_highlighting(new_code, vars)
    insert_start_logic + new_code
  end

  def process_code_for_precompile(code_msg)
    i = 1
    codes = ''
    code_msg.each_line do |line|
      codes += line.chomp + " % #{$prefix}_(#{i}#{$prefix}_)\n"
      i += 1
    end
    insert_compile_logic + codes
  end


  def postprocess_print(send, type, line)
    if type == 'precompile' #an error raised in compiling without our line-logic
      if @precompileflag
        @precompileflag = false
        send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
      end
      postprocess_error_compile(line) #handle compile-errors
    elsif type == 'precompileok' and @precompileflag #compiling without our line-logic succeeds
      send.call([{:write_file => {:filename => 'webpiraten.erl', :content => @process_code}},
                 {:execute => {:command => 'erlc -W0 webpiraten.erl', :stderr => 'compile', :stdout => 'compile', :permissions => 'read-write'}},
                 {:execute => {:command => 'echo ok', :stdout => 'ok'}}])
      {:type => :no}
    elsif type == 'compile' #compiling with our line logic failed, but without not => running in backup-mode
      if @compileflag
        @compileflag = false
        send.call([{:execute => {:command => 'erl -noshell -s prewebpiraten main -s init stop', :stderr => 'preerror'}}, {:exit => {}}])
        return {:type => :warning, :message => 'Start im vereinfachten Modus.'}
      end
      {:type => :no} #dismiss error message because it is not the users fault
    elsif type == 'ok' and @compileflag #compiling with our line logic succeeds
      send.call([{:execute => {:command => 'erl -noshell -s webpiraten main -s init stop'}}, {:exit => {}}])
      {:type => :no}
    elsif type == 'preerror' #handle runtime-error in backup-mode
      postprocess_error(line, 'prewebpiraten')
    elsif type == 'error' #handle runtime-error
      postprocess_error(line, 'webpiraten')
    else
      {:type => :no}
    end
  end

  # Processes a given error message, if the execution is aborted with an error.
  # If there are information about line numbers in the error message the method
  # will check if there is a corresponding line number which is visible to the
  # user.
  def postprocess_error(line, name)
    if name == 'webpiraten'
      regex_comp = /webpiraten\.erl:\d*:/
      code = @process_code
    else
      regex_comp = /prewebpiraten\.erl:\d*:/
      code = @precompile_code
    end
    if line =~ regex_comp # process message only if there's the filename
      index_begin = line.index(':') # find line number beginning
      index_end = line.index(':', index_begin+1) # find line number ending
      if index_begin && index_end && index_begin < index_end
        line_number = line[index_begin+1, index_end].to_i # extract error line number
        i = 1
        new_line_number = ''
        code.each_line do |lin|
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
        line.slice!("#{name}:")
      end
    end
    {:type => :error, :message => line}
  end

  # Processes a given error message, when the compiling ended with an error.
  # The handling and functionality is similar to postprocess_error.
  def postprocess_error_compile(line)
    if line =~ /prewebpiraten\.erl:\d*:/ # process message only if there's the filename
      index_begin = line.index(':') # find line number beginning
      index_end = line.index(':', index_begin+1) # find line number ending
      if index_begin && index_end && index_begin < index_end
        line_number = line[index_begin+1, index_end].to_i # extract error line number
        i = 1
        new_line_number = ''
        @precompile_code.each_line do |lin|
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
        line.slice!('prewebpiraten:')
      end
    end
    {:type => :error, :message => line}
  end

  # String that contains logic for the pirateships movements and also preprocesses
  # raised error messages in a pretty and easy understandable way in style of the
  # erlang-shell.
  def insert_start_logic
    %Q[
    -module(webpiraten).
    -export([main/0]).

    main() -> register(a#{$prefix}_debug,spawn(fun() -> a#{$prefix}_performdebugs() end)),
              try
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

    a#{$prefix}_break(F) -> io:fwrite("~n#{$prefix}_break_down~n"),
                            X = F(),
                            io:fwrite("~n#{$prefix}_break_up~n"),
                            X.

    a#{$prefix}_performdebugs() -> receive Index
                                     -> a#{$prefix}_performdebugs(Index)
                                   end.

    a#{$prefix}_performdebugs(Index) -> receive Value
                                          -> io:fwrite("~n#{$prefix}_debug_~p_~p~n", [Index, Value]),
                                             a#{$prefix}_performdebugs()
                                        end.

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

  # Contains
  def insert_compile_logic
    %Q[
    -module(prewebpiraten).
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

    move()  -> io:fwrite("~n#{$prefix}_move~n").

    take()  -> io:fwrite("~n#{$prefix}_take~n").

    puts()         -> puts(buoy).
    puts(buoy)     -> io:fwrite("~n#{$prefix}_put_buoy~n");
    puts(treasure) -> io:fwrite("~n#{$prefix}_put_treasure~n").

    turn()      -> turn(back).
    turn(back)  -> io:fwrite("~n#{$prefix}_turn_back~n");
    turn(right) -> io:fwrite("~n#{$prefix}_turn_right~n");
    turn(left)  -> io:fwrite("~n#{$prefix}_turn_left~n").

    look()      -> look(here).
    look(here)  -> io:fwrite("~n#{$prefix}_?_look_here~n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(front) -> io:fwrite("~n#{$prefix}_?_look_front~n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(left)  -> io:fwrite("~n#{$prefix}_?_look_left~n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(right) -> io:fwrite("~n#{$prefix}_?_look_right~n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(back)  -> io:fwrite("~n#{$prefix}_?_look_back~n"),
                   lists:nth(1,element(2,io:fread("", "~a"))).
]

  end
end
