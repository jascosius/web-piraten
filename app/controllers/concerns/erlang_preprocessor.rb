# -*- encoding : utf-8 -*-
class ErlangPreprocessor < BasePreprocessor

  attr :filename
  attr :compile
  attr :execute
  attr :compile_error
  attr :execute_error

  def initialize(attribut)
    super(attribut)
    @filename = "webpiraten.erl"
    @compile = 'cd $PATH$ && erlc webpiraten.erl' #'cd /codetemp && erl -make'
    @execute = 'cd $PATH$ && erl -s webpiraten main' #'echo "" && cd $PATH$ && sudo -u sailor erl -run webpiraten' #'cd $PATH$ && erl -run webpiraten'
    @compile_error = ''
    @execute_error = ''
  end

  def process_code(code_msg, vars)
    insert_start_logic + code_msg
  end

  def postprocess_error(line, _)
    line
  end

  def postprocess_error_compile(line, _)
    line
  end

  def insert_start_logic
    '
    -module(webpiraten).
    -export([main/0]).
    main()->start(), halt().
'
  end

end
