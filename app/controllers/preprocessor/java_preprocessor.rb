# -*- encoding : utf-8 -*-
class JavaPreprocessor

  attr :line_first

  def initialize(code, _)
    @line_first = true
    @process_code = process_code(code)
  end

  def commands_for_vm
    [{:write_file => {:filename => 'Pirate.java', :content => @process_code}},
     {:execute => {:command => 'javac -cp $LIB$/java $PATH$/Pirate.java', :permissions => 'read-write'}},
     {:execute => {:command => 'java -cp $PATH$:$LIB$/java Pirate'}},
     {:exit => {}}]
  end

  def process_code(code_msg)
    insert_logic + code_msg + insert_logic_end + "\n"
  end

  def postprocess_print(_, _, line)
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