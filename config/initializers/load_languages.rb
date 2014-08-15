require 'languages/language'
LANGUAGES = {
    :Ruby => Language.new('Ruby', ['codemirror/modes/ruby'], {:codemirror => {:mode => 'ruby'}}),
    :Erlang => Language.new('Erlang', ['codemirror/modes/erlang'], {:codemirror => {:mode => 'erlang'}}),
    :Java => Language.new('Java', ['codemirror/modes/clike'], {:codemirror => {:mode => 'clike'}})
}
#current_language = LANGUAGES[params.lang]