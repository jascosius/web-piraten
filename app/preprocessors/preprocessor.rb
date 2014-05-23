module Preprocessor

  require 'ruby_preprocessor'
  require 'java_preprocessor'
  attr_accessor :code

=begin
commit message
    The preprocessor (which eventually is a handler for redirecting the code to the right language specific preprocessor) as well as an abstract implementation of a base preprocessor and an implemented version of the ruby preprocessor and a dummy of the java preprocessor are now added. They moved to another sub-directory within the app folder but outside the controllers folder.
=end


  private # TODO is that really necessary?

# A method that takes the given message aka the code of the user and checks which
# programming language is selected as well as if the debug mode is set or not.
# Default set ist no debug mode and Ruby. The method then commits the code to the
# specified preprocessor.
  def preprocess_code(msg, language='Ruby', debug=false)
    case language
      when 'Ruby'
        ruby = RubyPreprocessor.new('Ruby')
        @code = debug ? ruby.debug_code(msg, var=[]) : ruby.process_code(msg)
      when 'Java'
        java = JavaPreprocessor.new('Java')
        @code =debug ? java.debug_code(msg, var=[]) : java.preprocess_code
      else
        @code = 'Something went terribly wrong!'
    end
    @code
  end

end