class Language
  attr_reader :name, :scripts, :options
  def initialize(name, scripts, options)
    @name = name
    @scripts = scripts
    @options = options
  end
  def getName
    @name
  end
end