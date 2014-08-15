require 'languages/language'

LANGUAGES = {
    # object => Language.new(name, [scrips], options, url)
    :ruby => Language.new('Ruby' , ['codemirror/modes/ruby'], {:codemirror => {:mode => 'ruby'}}),
    :erlang => Language.new('Erlang', ['codemirror/modes/erlang'], {:codemirror => {:mode => 'erlang'}}),
    :java => Language.new('Java', ['codemirror/modes/clike'], {:codemirror => {:mode => 'clike'}})
}
#current_language = LANGUAGES[params.lang]