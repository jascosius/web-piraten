# -*- encoding : utf-8 -*-

# a class containing the information around programming languages the system supports
class Language
  attr_accessor :preprocessor, # the class of the preprocessor the language uses, required
                :name, # name of the language, required
                :script_assets, # JavaScript assets that the client should load
                :gui_options, # settings for the client initialization
                :default_code, # default code in the editor of the client
                :file_extension, # file extension for code downloads (saving)
                :stylesheet_assets # CSS assets that the client should load


  def initialize(options)
    @preprocessor = options[:preprocessor]
    @name = options[:name]
    @script_assets = options[:script_assets] || []
    @gui_options = options[:gui_options] || {}
    @default_code = options[:default_code] || ''
    @file_extension = options[:file_extension] || '.txt'
    @stylesheet_assets = options[:stylesheet_assets] || []
  end
end
