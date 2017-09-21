# -*- encoding : utf-8 -*-
# config for the different languages

require 'languages/language'
require 'preprocessor/erlang/erlang_preprocessor'
require 'preprocessor/java/java_preprocessor'
require 'preprocessor/python/python_preprocessor'
require 'preprocessor/ruby/ruby_preprocessor'

# load the languages the system supports
LANGUAGES = {
    # object => Language.new(name, [scrips], options, url)
    :ruby => Language.new({
                              preprocessor: RubyPreprocessor,
                              name: 'Ruby',
                              script_assets: ['codemirror/modes/ruby.js'],
                              gui_options: {:codemirror => {:mode => 'ruby'}},
                              default_code: "puts 'Ahoy!'\n"+
                                  "while not (look(:front) == :border)\n"+
                                  "  move()\n"+
                                  "  turn(:left)\n"+
                                  "  turn(:right)\n"+
                                  "end\n"+
                                  "put()",
                              file_extension: 'rb'
                          }),
    :python => Language.new({
                              preprocessor: PythonPreprocessor,
                              name: 'Python',
                              script_assets: ['codemirror/modes/python.js'],
                              gui_options: {:codemirror => {:mode => 'python'}},
                              default_code: "print('Ahoy!')\n"+
                                  "while look(Dir.FRONT) is not Obj.BORDER:\n"+
                                  "    move()\n"+
                                  "    turn(Dir.LEFT)\n"+
                                  "    turn(Dir.RIGHT)\n"+
                                  "put()",
                              file_extension: 'py'
                          }),
    :erlang => Language.new({
                              preprocessor: ErlangPreprocessor,
                              name: 'Erlang',
                              script_assets: ['codemirror/modes/erlang.js'],
                              gui_options: {:codemirror => {:mode => 'erlang', :theme => 'blackboard'}},
                              default_code: "start() -> move(), turn(right), turn(left), prove().\n" +
                                  "prove() -> case look(front) of\n"+
                                  "             border -> puts();\n"+
                                  "             _      -> start()\n"+
                                  "           end.",
                              file_extension: 'erl'
                            }),
    :java => Language.new({
                              preprocessor: JavaPreprocessor,
                              name: 'Java',
                              script_assets: ['codemirror/modes/clike.js'],
                              #stylesheet_assets: ['codemirror/themes/eclipse.css'],
                              gui_options: {:codemirror => {:mode => 'text/x-java'}},
                              default_code: "public void start() {\n"+
                                  "  while(ship.look(Direction.FRONT) != Item.BORDER) {\n"+
                                  "    ship.move();\n"+
                                  "    for(int i = 0; i<2; i++) {\n"+
                                  "      ship.turn(Direction.RIGHT);\n"+
                                  "    }\n"+
                                  "    ship.move();\n"+
                                  "    ship.turn();\n"+
                                  "    ship.move();\n"+
                                  "    if (ship.look() == Item.TREASURE) {\n"+
                                  "      ship.take();\n"+
                                  "      ship.put();\n"+
                                  "    }\n"+
                                  "  }\n"+
                                  "}",
                              file_extension: 'java'
                          })
}
#current_language = LANGUAGES[params.lang]
