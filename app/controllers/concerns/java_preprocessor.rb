# -*- encoding : utf-8 -*-
class JavaPreprocessor < BasePreprocessor

  attr :line_first

  def initialize(attribut)
    super(attribut)
    @line_first = true
  end

  def commands_for_vm(code, tracing_vars)
    puts a = process_code(code, tracing_vars)
    [{:write_file => {:filename => 'Pirate.java', :content => process_code(code, tracing_vars)}},
     {:execute => {:command => 'javac -cp $LIB$/java $PATH$/Pirate.java', :permissions => 'high'}},
     {:execute => {:command => 'java -cp $PATH$:$LIB$/java Pirate'}},
     {:exit => {}}]
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

  def postprocess_print(_, _, line, _)
    {:type => :error, :message => line}
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