module Preprocessor

#  require 'ruby_preprocessor'
#  require 'java_preprocessor'


# A method that processes the code of the user to... TO DO
  def preprocess_code(msg, language='Ruby', debug=false)
    case language
      when 'Ruby'
        code = debug ? ruby_debug(msg) : ruby_preprocess(msg)
#        when 'Java'
#          processor = JavaPreprocessor.new('Java')
#          debug ? code = processor.debug_code : code = processor.preprocess_code
      else
        code = 'Something went terribly wrong!'
    end
    code
  end

  private

  def ruby_preprocess(code_msg)
    i=0
    codes = ''
    code_msg << "\n"
    code_msg.each_line do |s|
      codes += s + "line(#{i})\n"
      i += 1
    end
    ship_logic = "def move\n  puts \"move\"\nend\ndef turnRight\n  puts \"turnRight\"\nend\ndef turnLeft\n  puts \"turnLeft\"\nend\ndef addBuoy\n  puts \"addBuoy\"\nend\ndef line(i)\n  puts \"\\nline?\#{i}\"\nend\n\n"
    ship_logic + codes + "\n"
  end

  def ruby_debug(code_msg)
    i=0
    codes = ''
    code_msg.each_line do |s|
      codes += s + "line(#{i})\n"
      i += 1
    end
    ship_logic = "\ndef move\n      puts \"move\"\n    end\n    def turnRight\n      puts \"turnRight\"\n    end\n    def turnLeft\n      puts \"turnLeft\"\n    end\n    def addBuoy\n      puts \"addBuoy\"\n    end\n    def line(i)\n      puts \"\\nline?#{i}\"\n    end\n"
    ship_logic + codes + "\n"
  end

end