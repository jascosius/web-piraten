# -*- encoding : utf-8 -*-
require 'languages/language'
require 'preprocessor/ruby/ruby_preprocessor'
require 'preprocessor/java/java_preprocessor'
require 'preprocessor/erlang/erlang_preprocessor'

LANGUAGES = {
    # object => Language.new(name, [scrips], options, url)
    :ruby => Language.new({
        preprocessor: RubyPreprocessor,
        name: 'Ruby',
        script_assets: ['codemirror/modes/ruby.js'],
        gui_options: {:codemirror => {:mode => 'ruby'}},
        default_code: "print \"Hallo Welt!\"\n"+
        "i = 0\n"+
        "(1..3).each do\n"+
        "    print \"Durchlauf \#{i}\"\n"+
        "    move\n"+
        "    turn(:right)\n"+
        "    turn(:left)\n"+
        "    turn(:back)\n"+
        "    turn\n"+
        "    look\n"+
        "    look(:right)\n"+
        "    look(:left)\n"+
        "    look(:back)\n"+
        "    look(:here)\n"+
        "    look(:front)\n"+
        "    put\n"+
        "    take\n"+
        "    i += 1\n"+
        "end",
        file_extension: 'rb'
    }),
    :erlang => Language.new({
        preprocessor: ErlangPreprocessor,
        name: 'Erlang',
        script_assets: ['codemirror/modes/erlang.js'],
        gui_options: {:codemirror => {:mode => 'erlang'}},
        default_code: "start() -> case look(front) of\n" +
        "     treasure -> move(), take(), start(), puts(), move();\n" +
        "     _        -> case look(right) of\n" +
        "                   treasure -> turn(right), move(), take(), start(), puts(), move(), turn(left);\n" +
        "                   _        -> case look(left) of\n" +
        "                                 treasure -> turn(left), move(), take(), start(), puts(), move(), turn(right);\n" +
        "                                 _        -> turn()\n" +
        "                               end\n" +
        "                end\n" +
        "    end.\n",
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
        file_extension: 'java'})
}
#current_language = LANGUAGES[params.lang]
