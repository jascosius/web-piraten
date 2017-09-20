# -*- encoding : utf-8 -*-
# Preprocessor for Python programs.
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

    # Variables whose vaues are to be traced through the program's execution
    @tracing_vars = tracing_vars

    # If compilation fails, postprocess_print is called once for each line of output
    # the compile script generates. We only want to send the exit command once, though.
    # This flag controls whether we have already sent an abort
    @sent_exit_upon_compile_error = false

    @syntaxflag = true
  end



  ######
  #     # #####  ###### #####  #####   ####   ####  ######  ####   ####   ####  #####
  #     # #    # #      #    # #    # #    # #    # #      #      #      #    # #    #
  ######  #    # #####  #    # #    # #    # #      #####   ####   ####  #    # #    #
  #       #####  #      #####  #####  #    # #      #           #      # #    # #####
  #       #   #  #      #      #   #  #    # #    # #      #    # #    # #    # #   #
  #       #    # ###### #      #    #  ####   ####  ######  ####   ####   ####  #    #

  # Commands to be sent to the VM to check whether there are syntax errors in the code
  def commands_for_vm
    [{:write_file => {:filename => @filename, :content => @code}},
    {:execute => {:command => "#{VM_PYTHON} -B -m py_compile #{@filename} && echo success", :stdout => 'compilesuccess', :stderr => 'compileerror'}}]
    # in this case execution is a syntax check, real execution is handled in postprocess_print
  end


  # Handles output from the vm
  def postprocess_print(send, type, line)
    if type == 'compilesuccess'
      # The user's code does not contain any syntax errors that Python was able to catch. Process
      # the code and execute it.
      @process_code = process_code(@code, @tracing_vars)
      send.call([
        {:write_file => {:filename => @filename, :content => @process_code}},
        {:execute => {:command => "env PYTHONPATH=$LIB$/python #{VM_PYTHON} -B #{@filename}"}},
        {:exit => {}}])
      {:type => :no}

    elsif type == 'compileerror'
      # The code submitted by the user was erroneous. Tell the VM to exit once (upon encountering
      # the first line of complaints sent by the compiler) and then post-process the line to see
      # whether we should output anything to the user
      if not @sent_exit_upon_compile_error
        send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
        @sent_exit_upon_compile_error = true
      end
      return {:type => :error, :message => line}

    else
      # This has nothing to do with the syntax check anymore... Shit just got real!
      postprocess_execute(line)
    end
  end


  # Process any kind of message that might happen during execution. This may be messages generated
  # by the user, but it might just as well be things that went wrong during execution, such as
  # functions called with wrong arguments and stuff.
  def postprocess_execute(line)
    # TODO Process things
    line
  end



   #####                          #     #
  #     #  ####  #####  ######    ##   ##   ##   #    #  ####  #      # #    #  ####
  #       #    # #    # #         # # # #  #  #  ##   # #    # #      # ##   # #    #
  #       #    # #    # #####     #  #  # #    # # #  # #      #      # # #  # #
  #       #    # #    # #         #     # ###### #  # # #  ### #      # #  # # #  ###
  #     # #    # #    # #         #     # #    # #   ## #    # #      # #   ## #    #
   #####   ####  #####  ######    #     # #    # #    #  ####  ###### # #    #  ####

  # Add everything to the code that is required to get it to execute and to support different features
  # of the programming environment.
  def process_code(code, tracing_vars)
    return generate_header() + code
  end


  # Generates the Python header necessary to run pirate ships.
  def generate_header()
    %Q[
      from webpiraten import Dir
      from webpiraten import Obj

      from webpiraten import configure_prefix
      from webpiraten import look
      from webpiraten import move
      from webpiraten import put
      from webpiraten import take
      from webpiraten import turn

      configure_prefix("#{VM_PREFIX}")

    ]
  end

end
