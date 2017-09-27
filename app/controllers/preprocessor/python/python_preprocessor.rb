# -*- encoding : utf-8 -*-
# Preprocessor for Python programs. The processor goes through a number of phases.
#   1. :syntaxcheck
#      The first phase is all about checking whether Python likes the syntax of the
#      user's submission. We copy the code over to the server as is (that is, without
#      including any of the boiler plate code necessary to reference libraries and
#      things) and ask Python to compile it. If the code passes compilation, we move
#      on to the next phase. If it doesn't, we output any problems and cancel the
#      whole thing.
#
#   2. :augment
#      For the second phase, we augment the submitted code by adding things that
#      support various features of the online environment. We then check whether
#      the code still compiles. If it does so, we move on to the next phase using
#      the augmented code. If it does not, we move on to the next phase using the
#      original code after printing a corresponding hint to the console.
#
#   3. :execute
#      This is where the code is actually run and we process any output produced.
#      There are two kinds of output: Python-generated error messages, or things
#      users print to the console.
class PythonPreprocessor

  require 'preprocessor/python/augmentation'


  ###
   #  #    # # ##### #   ##   #      # ######   ##   ##### #  ####  #    #
   #  ##   # #   #   #  #  #  #      #     #   #  #    #   # #    # ##   #
   #  # #  # #   #   # #    # #      #    #   #    #   #   # #    # # #  #
   #  #  # # #   #   # ###### #      #   #    ######   #   # #    # #  # #
   #  #   ## #   #   # #    # #      #  #     #    #   #   # #    # #   ##
  ### #    # #   #   # #    # ###### # ###### #    #   #   #  ####  #    #

  # This flag needs to be set, so the communication between server
  # and client knows, if the packets are sent at the end of the
  # line of user's code or at the beginning.
  attr :line_first

  # Initialize the preprocessor
  def initialize(code, tracing_vars)
    @line_first = true

    # The name of the Python file we're going to put the user code into
    @filename = "pyraten.py"

    # The user's code
    @code = code

    # This will later hold our augmented version of the user's code
    @augmented_code = nil

    # Variables whose vaues are to be traced through the program's execution
    @tracing_vars = tracing_vars

    # If we encounter an error message, we need to send the exit command. An error
    # message consists of several lines that describe the error, but we only want to
    # send the exit message once.
    @exit_sent = false

    # For each user submission, we run through a number of phases as described in the
    # file header comment. This is the variable that tracks the current phase.
    @phase = :syntaxcheck

    # Error line processing.
    @drop_next_error_line = false
  end



  ######
  #     # #####  ###### #####  #####   ####   ####  ######  ####   ####   ####  #####
  #     # #    # #      #    # #    # #    # #    # #      #      #      #    # #    #
  ######  #    # #####  #    # #    # #    # #      #####   ####   ####  #    # #    #
  #       #####  #      #####  #####  #    # #      #           #      # #    # #####
  #       #   #  #      #      #   #  #    # #    # #      #    # #    # #    # #   #
  #       #    # ###### #      #    #  ####   ####  ######  ####   ####   ####  #    #

  # Commands to be sent to the VM to check whether there are syntax errors in the code. The
  # Python call is more complex than one might anticipate since we need to prevent Python
  # from trying to create __pycache__ folders, which the user that executes this code does
  # not necessarily have permission to do.
  def commands_for_vm
    [{:write_file => {:filename => @filename, :content => @code}},
    {:execute => {:command => "#{VM_PYTHON} -c \"compile(open('#{@filename}').read(), '', 'exec')\" && echo success",
                  :stdout => 'syntaxchecksuccess',
                  :stderr => 'syntaxcheckfail'}}]
    # in this case execution only triggers the initial syntax check, the rest is handled
    # in postprocess_print
  end

  # Processes output from the VM. This method simply delegates to more specific handlers.
  def postprocess_print(send, type, line)
    if type == "syntaxchecksuccess"
      return syntax_check_success(send, line)

    elsif type == "syntaxcheckfail"
      return syntax_check_fail(send, line)

    elsif type == "augmentsuccess"
      return augment_success(send, line)

    elsif type == "augmentfail"
      return augment_fail(send, line)

    elsif type == "executeoutput"
      return execute_output(send, line)

    elsif type == "executeerror"
      return execute_error(send, line)
    end
  end



   #####
  #     # #   # #    # #####   ##   #    #  ####  #    # ######  ####  #    #
  #        # #  ##   #   #    #  #   #  #  #    # #    # #      #    # #   #
   #####    #   # #  #   #   #    #   ##   #      ###### #####  #      ####
        #   #   #  # #   #   ######   ##   #      #    # #      #      #  #
  #     #   #   #   ##   #   #    #  #  #  #    # #    # #      #    # #   #
   #####    #   #    #   #   #    # #    #  ####  #    # ######  ####  #    #

  # Handles the case where the initial syntax check was successful. Move to the next phase, augment
  # the code and trigger a syntax check of the augmented code.
  def syntax_check_success(send, line)
    @phase = :augment
    @augmented_code = PythonCodeAugmenter.new(@code, @tracing_vars).to_a()

    # Trigger the new syntax check
    send.call([
      {:write_file => {:filename => @filename, :content => @augmented_code}},
      {:execute => {:command => "#{VM_PYTHON} -c \"compile(open('#{@filename}').read(), '', 'exec')\" && echo success",
                    :stdout => 'augmentsuccess',
                    :stderr => 'augmentfail'}}])

    # Don't output anything
    return {:type => :no}
  end

  # Handles the case where the initial syntax check has failed. This terminates the whole process
  # on the first invocation of this method and outputs the error message (which will usually be
  # distributed over several invocations of this method).
  def syntax_check_fail(send, line)
    # Send the exit command if that hasn't already happened
    if not @exit_sent
      send.call([{:exit => {:successful => false}}])
      @exit_sent = true
    end

    # Output the error line
    return error_line(line)
  end



     #
    # #   #    #  ####  #    # ###### #    # #####
   #   #  #    # #    # ##  ## #      ##   #   #
  #     # #    # #      # ## # #####  # #  #   #
  ####### #    # #  ### #    # #      #  # #   #
  #     # #    # #    # #    # #      #   ##   #
  #     #  ####   ####  #    # ###### #    #   #

  # Handles the case when our augmented code has passed syntax validation. Move to the next phase,
  # make the augmented code runnable, and trigger running it.
  def augment_success(send, line)
    @phase = :execute
    @augmented_code = make_runnable(@augmented_code)

    begin
      open("log/augmented_code.log", 'a') do |file|
        file.puts '--------------------------------'
        file.puts Time.now
        file.puts '--------------------------------'
        file.puts @augmented_code
        file.puts "\n\n\n"
      end
    ensure
      send.call([
        {:write_file => {:filename => @filename, :content => @augmented_code}},
        {:execute => {:command => "env PYTHONPATH=$LIB$/python #{VM_PYTHON} -B #{@filename}",
                      :stdout => 'executeoutput',
                      :stderr => 'executeerror'}}])
      # Don't output anything
      return {:type => :no}
    end
  end

  # Handles the case whn our augmented code has failed syntax validation. Move to the next phase,
  # make the original code runnable, and trigger running it.
  def augment_fail(send, line)
    @phase = :execute
    @augmented_code = nil
    @code = make_runnable(@code)

    begin
      open("log/simplemode.log", 'a') do |file|
        file.puts '--------------------------------'
        file.puts Time.now
        file.puts '--------------------------------'
        file.puts @code
        file.puts "\n\n\n"
      end
    ensure
      send.call([
        {:write_file => {:filename => @filename, :content => @code}},
        {:execute => {:command => "env PYTHONPATH=$LIB$/python #{VM_PYTHON} -B #{@filename}",
                      :stdout => 'executeoutput',
                      :stderr => 'executeerror'}}])
      return {:type => :warning, :message => 'Start im vereinfachten Modus.'}
    end
  end



  #######
  #       #    # ######  ####  #    # ##### ######
  #        #  #  #      #    # #    #   #   #
  #####     ##   #####  #      #    #   #   #####
  #         ##   #      #      #    #   #   #
  #        #  #  #      #    # #    #   #   #
  ####### #    # ######  ####   ####    #   ######

  # Handles output received on stdout during execution. Simply print the lines onto the console.
  def execute_output(send, line)
    return {:type => :log, :message => line}
  end

  # Handles output received on stderr during execution. These will be Python exceptions. On the first call,
  # we issue the exit command. The remaining calls process the remaining lines in the error output.
  def execute_error(send, line)
    # Send the exit command if that hasn't already happened
    if not @exit_sent
      send.call([{:exit => {:successful => false}}])
      @exit_sent = true
    end

    # Output the error line
    return error_line(line)
  end



  #     #
  #     # ##### # #      # ##### # ######  ####
  #     #   #   # #      #   #   # #      #
  #     #   #   # #      #   #   # #####   ####
  #     #   #   # #      #   #   # #           #
  #     #   #   # #      #   #   # #      #    #
   #####    #   # ###### #   #   # ######  ####

  # Turns the code into code that is actually runnable by inserting the required imports and things.
  def make_runnable(code)
    return %Q[
import webpiraten as garbledwebpiratenlibraryname
from webpiraten import Dir
from webpiraten import Obj
from webpiraten import look
from webpiraten import move
from webpiraten import put
from webpiraten import take
from webpiraten import turn

garbledwebpiratenlibraryname.configure_prefix("#{VM_PREFIX}")

] + code
  end


  # Python will generally generate two types of error messages. The first is during syntax checks:
  #
  #     Traceback (most recent call last):
  #       File "<string>", line 1, in <module>
  #       File "", line 2
  #         while look(Dir.FRONT) is not Obj.BORDER
  #                                               ^
  #     SyntaxError: invalid syntax
  #
  # Here, we throw away the first two lines completely, use the line information in the third line
  # to display a more helpful error message (possibly converting line numbers in the process),
  # output the fourth and fifth line as is and throw away the last line.
  #
  # The second is during execution:
  #
  #     Traceback (most recent call last):
  #       File "pyraten.py", line 17, in <module>
  #         turn(Dir.HERE)
  #       File "/home/captain/vm/lib/python/webpiraten.py", line 141, in turn
  #         raise ValueError("turn(...) erlaubt nur Dir.LEFT, Dir.RIGHT und Dir.BACK.")
  #     ValueError: turn(...) erlaubt nur Dir.LEFT, Dir.RIGHT und Dir.BACK.
  #
  # Here, we throw away the first line completely. The line that has "pyraten.py" in it is turned
  # into something more helpful (possibly converting line numbers in the process), and the line
  # following it immediately is printed as is. All the remaining lines are thrown away until we
  # find one that is not indented, which we print as is.


  # Takes a line of an error message and makes it ready to be presented to the user.
  def error_line(line)
    # A previously processed error line may cause us to drop the current one
    if @drop_next_error_line
      @drop_next_error_line = false
      return {:type => :no}
    end

    stripped_line = line.strip

    # We always throw away lines that start with certain prefixes
    throw_away_prefixes = ["Traceback ", 'File "<string>"', "SyntaxError: "]
    if stripped_line.start_with?(*throw_away_prefixes)
      return {:type => :no}
    end

    # Recognize the syntax error message
    match = /File \"\", line (\d+)/.match(stripped_line)
    if match
      # Note that at this point, we haven't inserted any code yet, so there's no need to convert
      # any line numbers
      return {:type => :error, :message => "Syntaxfehler in Zeile #{match[1]}:"}
    end

    # Recognize the first runtime error message
    match = /File \"pyraten.py\", line (\d+), in <module>/.match(stripped_line)
    if match
      # We need to convert line numbers
      ln = original_line_number(match[1]).to_s
      return {:type => :error, :message => "Laufzeitfehler in Zeile #{ln}:"}
    end

    # Recognize further runtime error messages
    match = /File \"[^\"]+\", line (\d+), in turn/.match(stripped_line)
    if match
      # Ignore this and the next line
      @drop_next_error_line = true
      return {:type => :no}
    end

    # Recognize the syntax error marker
    if stripped_line == "^"
      return {:type => :error, :message => line}
    end

    # It is some other line; be sure to remove any trailing end-of-line comment
    return {:type => :error, :message => PythonCodeAugmenter.remove_line_number(line)}
  end

  # Given a line number from an error message, try to find out which line number it would
  # correspond to in the user's original code. If we're in the syntax check phase, we haven't
  # done anything to the code yet, so the original line number equals the new line number.
  # If we are in the augmentation phase, this method shouldn't be called. If we're in the
  # execution phase, what we do depends on whether we're running augmented code or not. If
  # so, we look for a line number comment in the augmented code. Otherwise, we won't find
  # an original line number, but all we've done to the user's code was prepend a few lines,
  # so we subtract the number of prepended lines.
  def original_line_number(new_line)
    # First of all, convert the line number to an integer
    new_line = new_line.to_i

    if @phase == :execute
      # It's the execute phase, so we have more to do
      if @augmented_code.nil?
        # We haven't augmented the code with line numbers or anything
        number_of_prepended_lines = make_runnable("").count("\n")
        return new_line - number_of_prepended_lines

      else
        # Pull the offending line out of the augmented code
        augmented_code_lines = @augmented_code.lines
        if new_line > augmented_code_lines.length
          # We were not able to find the line the error message refers to. Shouldn't happen.
          return "<unbekannt>"
        else
          # Check if the line has a line number comment
          ln = PythonCodeAugmenter.extract_line_number(augmented_code_lines[new_line - 1])
          if ln.nil?
            return "<unbekannt>"
          else
            return ln
          end
        end
      end

    else
      # It's not the execute phase, so we can simply return the line number
      return new_line
    end
  end

end
