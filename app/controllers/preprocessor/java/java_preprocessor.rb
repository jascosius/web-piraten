# -*- encoding : utf-8 -*-
class JavaPreprocessor

  attr :line_first

  def initialize(code, _)
    @line_first = true
    @process_code = process_code(code)
    @compileflag = true
    @errorflag = true
  end

  def commands_for_vm
    [{:write_file => {:filename => 'Pirate.java', :content => @process_code}},
     {:execute => {:command => 'javac -cp $LIB$/java $PATH$/Pirate.java', :stderr => 'compile', :permissions => 'read-write'}},
     {:execute => {:command => 'echo ok', :stdout => 'ok'}}]
  end

  def process_code(code_msg)
    insert_logic + code_msg + insert_logic_end + "\n"
  end

  def postprocess_print(send, type, line)
    if type == 'compile' and @errorflag
      if @compileflag
        @compileflag = false
        send.call([{:exit => {:successful => false, :message => 'Syntaxfehler'}}])
      end
      postprocess_error_compile(line) #handle compile-errors
    elsif type == 'ok' and @compileflag
      send.call([{:execute => {:command => 'java -cp $PATH$:$LIB$/java Pirate'}},
                 {:exit => {}}])
      {:type => :no}
    elsif type == 'ok'
      @errorflag = false
      {:type => :no}
    elsif type == 'error'
      postprocess_error(line)
    else
      {:type => :no}
    end
  end

  def postprocess_error(line)
    if line.index('at logic.')
      line.slice!('logic.')
      puts point = line.index('.')
      at = line.index('at ')
      if point and at
        line.slice!(at+3..point)
      end
      first_brackets = line.index('(')
      if first_brackets
        line = line[0..first_brackets-1]
      end
    end
    if line.index('at Pirate.')
      first_colon = line.index(':')
      last_bracket = line.index(')',first_colon)
      line_number = line[first_colon+1..last_bracket-1].to_i
      if line_number <= 12 #lines of insert_logic
        return {:type => :no}
      end
      line.slice!(first_colon+1..last_bracket-1)
      line = line.insert(first_colon+1,(line_number-12).to_s) #lines of insert_logic
      line.slice!('Pirate.')
    end
    line.gsub!('Pirate.java','line')
    {:type => :error, :message => line}
  end

  def postprocess_error_compile(line)
    #remove filepath
    index_begin = line.index('/')
    index_end = line.index('Pirate.java:')
    if index_begin and index_end
      index_end += 11 #length of Pirate.java
      line.slice!(index_begin...index_end) #remove the filepath

      #change the linenumber
      index_line_end = line.index(':', index_begin+1)
      line_number = line[1...index_line_end]
      line.slice!(index_begin+1...index_line_end)
      new_line = line_number.to_i - 12 #lines of insert_logic
      line = line.insert(index_begin+1, new_line.to_s)
      line = line.insert(index_begin, 'line')
    end
    {:type => :error, :message => line}
  end


  # A method that stores the language- and ship-logic for Ruby that's put in the
  # code of the user to get the ship moving and so on.
  def insert_logic
    %q[
import logic.*;

public class Pirate{

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
