# -*- encoding : utf-8 -*-
class JavaPreprocessor < BasePreprocessor

  attr :filename
  attr :compile
  attr :execute
  attr :compile_error
  attr :execute_error

  def initialize(attribut)
    super(attribut)
    @filename = 'Pirate.java'
    @compile = 'javac -cp java $PATH$/Pirate.java' #$PATH$ will be replaced
    @execute = 'java -cp $PATH$:java Pirate'
    @compile_error = 'error' #break, when this is in the last line of compiler error
    @execute_error = 'Could not find or load main class' #break, when this is in the first line of the execution error
  end

  def process_code(code_msg, vars)
    i=1
    codes = ''
    code_msg.each_line do |s|
      codes += "#{$prefix}_line(#{i});\n" + s.chomp + " // #{$prefix}_(#{i+1}#{$prefix}_)\n"
      i += 1
    end
    codes.slice! "#{$prefix}_line(1);\n"

    insert_logic + codes + insert_logic_end + "\n"
  end

  def postprocess_error(line, _)
    line
  end

  def postprocess_error_compile(line, _)
    line
  end


  # A method that stores the language- and ship-logic for Ruby that's put in the
  # code of the user to get the ship moving and so on.
  def insert_logic
    %q[
import logic.*;

public class Pirate extends Main{

	public Ship ship;

	public static void main(String[] args) {
		Pirate pirate = new Pirate();
		pirate.ship = new Ship();
		pirate.start();
	}
]
  end



  def insert_logic_end
    '}'
  end

end