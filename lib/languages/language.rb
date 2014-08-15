class Language
  attr_accessor :name, :script_assets, :gui_options, :default_code, :file_extension
  def initialize(options)
    @name = options[:name]
    @script_assets = options[:script_assets] || []
    @gui_options = options[:gui_options] || {}
    @default_code = options[:default_code] || ''
    @file_extension = options[:file_extension] || '.txt'
  end
end