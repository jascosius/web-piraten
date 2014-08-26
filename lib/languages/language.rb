# -*- encoding : utf-8 -*-
class Language
  attr_accessor :preprocessor,
                :name,
                :script_assets,
                :gui_options,
                :default_code,
                :file_extension,
                :stylesheet_assets


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
