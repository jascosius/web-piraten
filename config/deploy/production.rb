# -*- encoding : utf-8 -*-
set :branch      , 'master'
set :application , 'web-piraten'
set :sub_uri     , '/web-piraten'
set :deploy_to   , "/var/lib/www-rails/#{fetch(:application)}"
set :rails_env   , :production
