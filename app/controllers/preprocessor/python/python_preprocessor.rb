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
    @augmented_code = augment_code(@code, @tracing_vars)

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
      send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
      @exit_sent = true
    end

    # Output the error line
    # TODO Handle error output differently
    return {:type => :error, :message => line}
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

    send.call([
      {:write_file => {:filename => @filename, :content => @augmented_code}},
      {:execute => {:command => "env PYTHONPATH=$LIB$/python #{VM_PYTHON} -B #{@filename}",
                    :stdout => 'executeoutput',
                    :stderr => 'executeerror'}}])

    # Don't output anything
    return {:type => :no}
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

  # Add our magic to the code that will enable different features of the development environment
  # to work.
  def augment_code(code, tracing_vars)
    # TODO: Actually augment the code here.
    return code
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
    return {:type => :no}
  end

  # Handles output received on stderr during execution. These will be Python exceptions. On the first call,
  # we issue the exit command. The remaining calls process the remaining lines in the error output.
  def execute_error(send, line)
    # Send the exit command if that hasn't already happened
    if not @exit_sent
      send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
      @exit_sent = true
    end

    # Output the error line
    # TODO Handle error output differently
    return {:type => :error, :message => line}
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
from webpiraten import Dir
from webpiraten import Obj

from webpiraten import configure_prefix
from webpiraten import look
from webpiraten import move
from webpiraten import put
from webpiraten import take
from webpiraten import turn

configure_prefix("#{VM_PREFIX}")

] + code
  end

end
