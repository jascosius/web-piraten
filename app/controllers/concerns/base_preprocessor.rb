# -*- encoding : utf-8 -*-
class BasePreprocessor
  attr_accessor :attrib

  # Initializes the class and checks if somebody wants to use the 'abstract' base class.
  def initialize(i_attrib)
    @attrib = i_attrib
    check_forbidden_instantiating
  end

  # Abstract base method for processing the code into the right logic for a
  # programming-language, needs to be overwritten.
  def process_code(msg)
    crash_for_abstract_violation('process_code')
  end

  def postprocess_error(line,code,file)
    crash_for_abstract_violation('postprocess error')
  end

  def preprocess_filename
    crash_for_abstract_violation('preprocess filename')
  end

  def preprocess_compile
    crash_for_abstract_violation('preprocess compile')
  end

  def preprocess_execute
    crash_for_abstract_violation('preprocess execute')
  end

  def preprocess_compile_error
    crash_for_abstract_violation('preprocess compile error')
  end

  def preprocess_execute_error
    crash_for_abstract_violation('preprocess execute error')
  end

  # Same as process_code but with the exception that there has to be a special
  # treatment for the debugging mode.
  def debug_code(msg, var_array)
    crash_for_abstract_violation('debug_code')
  end

  # If there is someone who wants to use the base class methods it raises an error,
  # so no one is able to use them unless he makes a new class that inherits the
  # methods.
  def crash_for_abstract_violation(self_method_name)
    raise 'You need to implement ' +
              "#{self.to_s}##{self_method_name} to use it!"
  end

  # If there is someone who wants to use the base class it raises an error, so
  # no one is able to use it.
  def check_forbidden_instantiating
    raise "Sorry! I'm not made for that use!" if self.to_s == 'BasePreprocessor'
  end

end
