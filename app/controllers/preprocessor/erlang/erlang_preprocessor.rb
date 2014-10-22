# -*- encoding : utf-8 -*-
# class that controls the execution of erlang
# handle the outputs
# and add the logic to the code of the user
# some of the used methods are defined in processing_tools.rb
# most of the regular expressions can be found in regular_expressions.rb
# (both files are in folder 'erlang')
class ErlangPreprocessor

  require 'preprocessor/erlang/processing_tools'

  attr :line_first

  def initialize(code, tracing_vars)
    @line_first = true
    @compileflag = true
    @precompileflag = true
    @code = code
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
     {:execute => {:command => 'erlc -o $LIB$/erlang/ -W0 prewebpiraten.erl', :stderr => 'precompile', :stdout => 'precompile', :permissions => 'read-write'}},
     {:execute => {:command => 'echo ok', :stdout => 'precompileok'}}]
  end

  # Processes the given code to add linehighlighting and so on
  def process_code(code_msg, vars)
    new_code = remove_comments(code_msg)
    new_code = insert_highlighting(new_code, vars) # method is found in processing_tools.rb
    insert_start_logic + new_code
  end

  # Processes the given code
  # just add really necessary functions
  # do not add advanced functions link linehighlighting
  def process_code_for_precompile(code_msg)
    i = 1
    codes = ''
    code_msg.each_line do |line|
      codes += line.chomp + " % #{$prefix}_(#{i}#{$prefix}_)\n"
      i += 1
    end
    insert_compile_logic + codes
  end

  # appending on the type of answers from the compiler
  # react like explained in the comment of commands_for_vm
  def postprocess_print(send, type, line)
    if type == 'precompile' #an error raised in compiling without our line-logic
      if @precompileflag
        @precompileflag = false
        send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
      end
      postprocess_error_compile(line)
    elsif type == 'precompileok' and @precompileflag #compiling without our line-logic succeeds
      send.call([{:write_file => {:filename => 'webpiraten.erl', :content => @process_code}},
                 {:execute => {:command => 'erlc -o $LIB$/erlang/ -W0 webpiraten.erl', :stderr => 'compile', :stdout => 'compile', :permissions => 'read-write'}},
                 {:execute => {:command => 'echo ok', :stdout => 'ok'}}])
      {:type => :no}
    elsif type == 'compile' #compiling with our line logic failed, but without not => running in backup-mode
      if @compileflag
        @compileflag = false
        begin
          open("log/simplemode.log", 'a') do |file|
            file.puts '--------------------------------'
            file.puts Time.now
            file.puts '--------------------------------'
            file.puts @code
            file.puts "\n\n\n"
          end
        ensure
          send.call([{:execute => {:command => 'erl -env ERL_LIBS "$LIB$/erlang/" -noshell -s prewebpiraten main -s init stop', :stderr => 'preerror'}}, {:exit => {}}])
          return {:type => :warning, :message => 'Start im vereinfachten Modus.'}
        end
      end
      {:type => :no} #dismiss error message because it is not the users fault
    elsif type == 'ok' and @compileflag #compiling with our line logic succeeds
      send.call([{:execute => {:command => 'erl -env ERL_LIBS "$LIB$/erlang/" -noshell -s webpiraten main -s init stop'}}, {:exit => {}}])
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
      index_begin = line.index(':')
      index_end = line.index(':', index_begin+1)
      if index_begin && index_end && index_begin < index_end
        line_number = line[index_begin+1, index_end].to_i
        i = 1
        new_line_number = ''
        code.each_line do |lin|
          # search for original line number in the user's code
          # and store it in new_line_number
          if i == line_number
            line_begin = lin.index("#{$prefix}_(")
            line_end = lin.index("#{$prefix}_)")
            if line_begin and line_end
              new_line_number = lin[line_begin+"#{$prefix}_(".length...line_end]
            end
          end
          i += 1
        end

        # remove old line number
        line.slice!(index_begin+1...index_end)

        if new_line_number == ''
          line.slice!(0..index_begin+2)
        else
          # add the new line number to the error and exchange filename with 'line'
          line = line.insert(index_begin+1, new_line_number)
          line = line.insert(index_begin, 'line')
          line.slice!(0...index_begin)
        end
        # remove specific module information in the error message for the used
        # module 'webpiraten' or 'prewebpiraten'
        line.slice!("#{name}:")
      end
    end
    {:type => :error, :message => line}
  end

  # Processes a given error message, when the compiling ended with an error.
  # The handling and functionality is similar to postprocess_error.
  def postprocess_error_compile(line)
    if line =~ /prewebpiraten\.erl:\d*:/ # process message only if there's the filename
      index_begin = line.index(':')
      index_end = line.index(':', index_begin+1)
      if index_begin && index_end && index_begin < index_end
        line_number = line[index_begin+1, index_end].to_i
        i = 1
        new_line_number = ''
        @precompile_code.each_line do |lin|
          # search for original line number in the user's code
          # and store it in new_line_number
          if i == line_number
            line_begin = lin.index("#{$prefix}_(")
            line_end = lin.index("#{$prefix}_)")
            if line_begin and line_end
              new_line_number = lin[line_begin+"#{$prefix}_(".length...line_end]
            end
          end
          i += 1
        end

        # remove old line number
        line.slice!(index_begin+1...index_end)

        if new_line_number == ''
          line.slice!(0..index_begin+2)
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
    -import(base,[print/1,putStr/1,putStrLn/1,show/1]).
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

    a#{$prefix}_break(F) -> io:fwrite("~n#{$prefix}_break_down~n"),
                            X = F(),
                            io:fwrite("~n#{$prefix}_break_up~n"),
                            X.

    a#{$prefix}_performdebugs(Index) -> receive Value
                                          -> io:fwrite("~n#{$prefix}_debug_~p_~p~n", [Index, Value])
                                        end.

    a#{$prefix}_performdebugs(Index, Value) -> io:fwrite("~n#{$prefix}_debug_~p_~p~n", [Index, Value]).

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

    break()      -> break(point).
    break(point) -> io:fwrite("~n#{$prefix}_break_point~n").

]
  end

  # Provides just ship logic for movements and interactions
  # No advanced functionality like line-highlighting etc.
  def insert_compile_logic
    %Q[
    -module(prewebpiraten).
    -import(base,[print/1,putStr/1,putStrLn/1,show/1]).
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

    break()      -> break(point).
    break(point) -> io:fwrite("~n#{$prefix}_break_point~n").
]

  end
end
