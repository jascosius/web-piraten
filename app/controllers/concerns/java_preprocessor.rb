# -*- encoding : utf-8 -*-
class JavaPreprocessor < BasePreprocessor

  attr :filename
  attr :compile
  attr :execute
  attr :compile_error
  attr :execute_error

  def initialize(attribut)
    super(attribut)
    @filename = 'Main.java'
    @compile = 'javac $PATH$/Main.java' #$PATH$ will be replaced
    @execute = 'java -cp $PATH$ Main'
    @compile_error = 'error' #break, when this is in the last line of compiler error
    @execute_error = '' #break, when this is in the first line of the execution error
  end

  def process_code(code_msg)
    insert_logic + code_msg + insert_logic_end + "\n"
  end

  def postprocess_error(line, code)
    line
  end

  def postprocess_error_compile(line,code)
    line
  end

  def debug_code(code_msg, vars)
  end

  # A method that stores the language- and ship-logic for Ruby that's put in the
  # code of the user to get the ship moving and so on.
  def insert_logic
    "import java.io.BufferedReader;\n" +
    "import java.io.IOException;\n" +
    "import java.io.InputStreamReader;\n" +

    "public class Main {\n" +

    "  private BufferedReader br;\n" +

    "  public static void main(String[] args) {\n" +
    "    new Main();\n" +
    "  }\n" +

    "  private String readLine() {\n" +
    "    try {\n" +
    "      return br.readLine();\n" +
    "    } catch (IOException e) {\n" +
    "      e.printStackTrace();\n" +
    "    }\n" +
    "  return \"\";\n" +
    "  }\n" +

    "  public Main() {\n" +
    "    InputStreamReader isr = new InputStreamReader(System.in);\n" +
    "    br = new BufferedReader(isr);\n" +
    "    start();\n" +
    "    try {\n" +
    "      br.close();\n" +
    "      isr.close();\n" +
    "    } catch (IOException e) {\n" +
    "      e.printStackTrace();\n" +
    "    }\n" +
    "  }\n" +

    "  public void move() {\n" +
    "    System.out.println(\"#{$prefix}move\");\n" +
    "  }\n" +

    "  public void turn(String dir) {\n" +
    "    switch(dir) {\n" +
    "    case \"right\":\n" +
    "      System.out.println(\"#{$prefix}turn_right\");\n" +
    "      break;\n" +
    "    case \"left\":\n" +
    "      System.out.println(\"#{$prefix}turn_left\");\n" +
    "      break;\n" +
    "    case \"over\":\n" +
    "      System.out.println(\"#{$prefix}turn_over\");\n" +
    "      break;\n" +
    "    }\n" +
    "  }\n" +

    "  public void put() {\n" +
    "    System.out.println(\"#{$prefix}put\");\n" +
    "  }\n" +

    "  public void line(int i) {\n" +
    "    System.out.println(\"#{$prefix}line!\" + i);\n" +
    "  }\n" +

    "  public String look(String dir) {\n" +
    "    switch(dir) {\n" +
    "    case \"right\":\n" +
    "      System.out.println(\"#{$prefix}?_look_right\");\n" +
    "      break;\n" +
    "    case \"left\":\n" +
    "      System.out.println(\"#{$prefix}?_look_left\");\n" +
    "      break;\n" +
    "    case \"here\":\n" +
    "      System.out.println(\"#{$prefix}?_look_here\");\n" +
    "      break;\n" +
    "    case \"back\":\n" +
    "      System.out.println(\"#{$prefix}?_look_back\");\n" +
    "      break;\n" +
    "    case \"front\":\n" +
    "      System.out.println(\"#{$prefix}?_look_front\");\n" +
    "      break;\n" +
    "    }\n" +
    "    String ret = readLine();\n" +
    "    if (ret.contains(\"#{$prefix}!_Buoy\")) {\n" +
    "      return \"buoy\";\n" +
    "    } else if (ret.contains(\"#{$prefix}!_Monster\")) {\n" +
    "      return \"monster\";\n" +
    "    } else if (ret.contains(\"#{$prefix}!_Treasure\")) {\n" +
    "      return \"treasure\";\n" +
    "    } else if (ret.contains(\"#{$prefix}!_Wave\")) {\n" +
    "      return \"wave\";\n" +
    "    } else {\n" +
    "      return \"nothing\";\n" +
    "    }\n" +
    "  }\n"
  end

  def insert_logic_end
    "}"
  end

end