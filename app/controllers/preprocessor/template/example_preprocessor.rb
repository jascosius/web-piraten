# -*- encoding : utf-8 -*-
# Template Class without real functionality to show the necessary
# methods for a new preprocessor. Methods are reduced methods of
# the ruby preprocessor.
class ExamplePreprocessor

  # This flag needs to be set, so the communication between server
  # and client knows, if the packets are sent at the end of the
  # line of user's code or at the beginning.
  attr :line_first

  # For example here the initializer of preprocessor for ruby.
  # The only attribute that needs to be set is @line_first, which
  # is accessed from other points outside the preprocessor.
  # Nontheless it's recommended to set instance_variables like
  # @code and flags like @syntaxflag for easier access inside
  # the preprocessor.
  def initialize(code, tracing_vars)
    @line_first = true
    @operationlist = []
    @end_break = ''
    @beg_break = ''
    @filename = "#{VM_PREFIX}_code.rb"
    @syntaxflag = true
    @code = code
    @tracing_vars = tracing_vars
    # @processed_code = process_code(code)
    # not
  end

  # Returns an array of commands (in hashes) for the VM. Here
  # for example it's the command array for ruby.
  # A list of the default-commands can be found in
  # /app/controllers/tools/commands_for_vm.rb
  def commands_for_vm
    [{:write_file => {:filename => @filename, :content => @code}},
     {:execute => {:command => "ruby -c #{@filename}", :stdout => 'checksuccess', :stderr => 'checkerror'}}]
    # in this case execution is a syntax check, real execution is
    # handled in postprocess_print
  end

  # This method handles the thrown errors of the user codes execution.
  # Here for example it's the handling for ruby.
  # If syntax check succeeds a call is send for execution of processed
  # code. Also if there is an error whilst syntax check the error is
  # processed by self defined function postprocess_execute
  def postprocess_print(send, type, line)
    if type == 'checksuccess'
      @process_code = process_code(@code, @tracing_vars)
      send.call([{:write_file => {:filename => @filename, :content => @process_code}}, {:execute => {:command => "ruby #{@filename}"}}, {:exit => {}}])
      {:type => :no}
    elsif type == 'checkerror'
      if @syntaxflag
        send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
        @syntaxflag = false
      end
      return {:type => :error, :message => line}
    else
      postprocess_execute(line)
    end
  end

  # Self defined method for message processing, no need to be called
  # postprocess_execute (free naming).
  def postprocess_execute(line)
    # here is the individual message processing located
    line
  end

  # Self defined method for processing of user code.
  # Handles insertion of debug- or linehighlighting-information.
  def process_code(code, tracing_vars)
    # insert information in code here
    tracing_vars
    code
  end

end
