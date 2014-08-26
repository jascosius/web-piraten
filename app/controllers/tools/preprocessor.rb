# -*- encoding : utf-8 -*-

class Preprocessor

  require 'tools/commands_for_vm'

  # A method that takes the given message aka the code of the user and checks which
  # programming language is selected as well as if the debug mode is set or not by
  # checking if there are any variables given the user wants to trace.
  # Default set ist no debug mode and Ruby. The method then commits the code to the
  # specified preprocessor and afterwards returns the modified code.
  def initialize(language,code,tracing_vars)
    @lang = LANGUAGES[language.to_sym].preprocessor.new(code,tracing_vars)
  end

  def commands_for_vm
    proof_commands(@lang.commands_for_vm)
  end

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

  def line_first
    @lang.line_first
  end

end
