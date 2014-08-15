require 'languages/language'

LANGUAGES = {
    # object => Language.new(name, [scrips], options, url)
    :ruby => Language.new({
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
        name: 'Erlang',
        script_assets: ['codemirror/modes/erlang.js'],
        options: {:codemirror => {:mode => 'erlang'}},
        default_code: '',
        file_extension: 'erl'}),
    :java => Language.new({
        name: 'Java',
        script_assets: ['codemirror/modes/clike.js'],
        gui_options: {:codemirror => {:mode => 'clike'}},
        default_code: '',
        file_extension: 'java'})
}
#current_language = LANGUAGES[params.lang]