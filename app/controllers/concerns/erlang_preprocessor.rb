# -*- encoding : utf-8 -*-
class ErlangPreprocessor < BasePreprocessor

  attr :filename
  attr :compile
  attr :execute
  attr :compile_error
  attr :execute_error
  attr :line_first

  def initialize(attribut)
    super(attribut)
    @filename = "webpiraten.erl"
    @compile = 'cd $PATH$ && erlc -W0 webpiraten.erl' #'cd /codetemp && erl -make'
    @execute = 'cd $PATH$ && erl -s webpiraten main' #'echo "" && cd $PATH$ && sudo -u sailor erl -run webpiraten' #'cd $PATH$ && erl -run webpiraten'
    @compile_error = ''
    @execute_error = ''
    @line_first = false
  end

  def process_code(code_msg, vars)
    codes = ''
    i = 2
    code_msg.each_line do |line|

      puts "line number #{i}"
      index_end = line.index('.')
      index_sem = line.index(';')
      index_com = line.rindex(',')
      index_sta = line.index('->')
      if index_sta
        index_sta += 2
        line = line.insert(index_sta, " a#{$prefix}_line(#{i}),")
      elsif index_end
        line = line.insert(index_end, ", a#{$prefix}_line(#{i})")
      elsif index_com
        line = line.insert(index_com, ", a#{$prefix}_line(#{i})")
      elsif index_sem
        line = line.insert(index_sem, ", a#{$prefix}_line(#{i})")
      end
      codes += line
      #codes += "#{$prefix}_line(#{i})\n" + line.chomp
      puts line
      i += 1
    end
    insert_start_logic + codes
  end

  def postprocess_error(line, _)
    line
  end

  def postprocess_error_compile(line, _)
    line
  end

#{$prefix}_line(i) -> io:fwrite('#{$prefix}_line_\#{i}\n').
  #{$prefix}_line(i) -> io:fwrite('CkyUHZVL3q_line_\#{i}\n').
  def insert_start_logic
    %Q[
    -module(webpiraten).
    -export([main/0]).

    main() -> start(), halt().

    a#{$prefix}_line(I) -> io:fwrite("\n#{$prefix}_line_" ++ integer_to_list(I)).

    move() -> io:fwrite('\n#{$prefix}_move\n').

    take() -> io:fwrite('\n#{$prefix}_take\n').

    put()         -> put(buoy).
    put(buoy)     -> io:fwrite('\n#{$prefix}_put_buoy\n');
    put(treasure) -> io:fwrite('\n#{$prefix}_put_treasure\n').

    turn()      -> turn(back).
    turn(back)  -> io:fwrite('\n#{$prefix}_turn_back\n');
    turn(right) -> io:fwrite('\n#{$prefix}_turn_right\n');
    turn(left)  -> io:fwrite('\n#{$prefix}_turn_left\n').

    look()      -> look(here).
    look(here)  -> io:fwrite("\n#{$prefix}_?_look_here\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(front) -> io:fwrite("\n#{$prefix}_?_look_front\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(left)  -> io:fwrite("\n#{$prefix}_?_look_left\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(right) -> io:fwrite("\n#{$prefix}_?_look_right\n"),
                   lists:nth(1,element(2,io:fread("", "~a")));
    look(back)  -> io:fwrite("\n#{$prefix}_?_look_back\n"),
                   lists:nth(1,element(2,io:fread("", "~a"))).
]
  end

end
