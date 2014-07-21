# -*- encoding : utf-8 -*-
require File.expand_path('../boot', __FILE__)

require 'rails/all'

# Require the gems listed in Gemfile, including any gems
# you've limited to :test, :development, or :production.
Bundler.require(*Rails.groups)

module RailsPrototype
  class Application < Rails::Application
    # Settings in config/environments/* take precedence over those specified here.
    # Application configuration should go into files in config/initializers
    # -- all .rb files in that directory are automatically loaded.

    # Set Time.zone default to the specified zone and make Active Record auto-convert to this zone.
    # Run "rake -D time" for a list of tasks for finding time zone names. Default is UTC.
    # config.time_zone = 'Central Time (US & Canada)'

    # The default locale is :en and all translations from config/locales/*.rb,yml are auto loaded.
    # config.i18n.load_path += Dir[Rails.root.join('my', 'locales', '*.{rb,yml}').to_s]
    # config.i18n.default_locale = :de
    config.i18n.default_locale = :de
    config.assets.precompile += [
        'twitter/bootstrap/glyphicons-halflings-regular.woff',
        'twitter/bootstrap/glyphicons-halflings-regular.ttf',
        'twitter/bootstrap/glyphicons-halflings-regular.eot',
        'twitter/bootstrap/glyphicons-halflings-regular.svg',
        'glyphicons-halflings-regular.woff',
        'glyphicons-halflings-regular.ttf',
        'glyphicons-halflings-regular.eot',
        'glyphicons-halflings.svg'
    ]
  end
end
