# -*- encoding : utf-8 -*-
module Preprocessor
  extend ActiveSupport::Concern

  require 'ruby_preprocessor'
  require 'java_preprocessor'

  attr_accessor :code

  private # TODO is that really necessary?

  # A method that takes the given message aka the code of the user and checks which
  # programming language is selected as well as if the debug mode is set or not.
  # Default set ist no debug mode and Ruby. The method then commits the code to the
  # specified preprocessor and afterwards returns the modified code.
  def preprocess_code(msg, language='Ruby', debug=false)
    case language
      when 'Ruby'
        ruby = RubyPreprocessor.new('Ruby')
        @code = debug ? ruby.debug_code(msg, var=[]) : ruby.process_code(msg)
      when 'Java'
        java = JavaPreprocessor.new('Java')
        @code =debug ? java.debug_code(msg, var=[]) : java.process_code(msg)
      else
        @code = 'Something went terribly wrong!'
    end
    @code
  end

end
