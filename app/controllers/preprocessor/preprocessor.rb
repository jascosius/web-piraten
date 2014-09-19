# -*- encoding : utf-8 -*-
# superclass for all preprocessors for a specific language
# each preprocessor needs at least the methods called here
class Preprocessor

  require 'tools/commands_for_vm'


  # Initializes a preprocessor for a given language.
  def initialize(language, code, tracing_vars)
    @lang = LANGUAGES[language.to_sym].preprocessor.new(code, tracing_vars)
  end

  # commands to initialize the vm
  def commands_for_vm
    @lang.commands_for_vm
  end

  # handle outputs from the vm, which aren't build in commands
  # send is a lambda-function to send new commands to the vm
  # type is the type of the output, set in the command
  # line is the line which was printed
  # the return value of postprocess_print in a concrete preprocessor must be a hash with a type of the message and the message its self
  def postprocess_print(send, type, line)
    result = @lang.postprocess_print(send, type, line)
    unless result.is_a?(Hash)
      $stderr.puts 'The result of \'postprocess_print/3\' must be a hash with key \':type\' (value one of :log, :warning, :error) and key \':message\'.'
      return
    end
    unless result[:type]
      $stderr.puts 'The result of \'postprocess_print/3\' must be a hash with key \':type\' (value one of :log, :warning, :error, :no) and key \':message\'.'
      return
    end
    unless [:log, :warning, :error, :no].include?(result[:type])
      $stderr.puts 'The value of \':type\' must be one of :log, :warning, :error, :no.'
      return
    end
    result
  end

  # says if the line-function is inserted at the beginning or at the end of a line
  def line_first
    @lang.line_first
  end

end
