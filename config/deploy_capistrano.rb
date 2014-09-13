# -*- encoding : utf-8 -*-
# ----------------------------------------------------------------------------
# Configuration
# ----------------------------------------------------------------------------

# config valid only for Capistrano 3.1
lock '3.2.1'

set :repo_url   , 'git@git-ps.informatik.uni-kiel.de:projekte/web-piraten.git'
set :scm        , :git

# Default branch is :master
# ask :branch, proc { `git rev-parse --abbrev-ref HEAD`.chomp }.call

# Deployment configuration
set :server   , 'giscours.informatik.uni-kiel.de'

role :web, fetch(:server)
role :app, fetch(:server)
role :db,  fetch(:server)

set :ssh_options, {
    :user          => 'www-rails',
    :port          => '55055'
}

# Ruby version
set :rvm_ruby_string  , 'ruby-2.0.0'
set :rvm_autolibs_flag, 'read-only'
set :rvm_type         , :user

# Default value for :format is :pretty
# set :format, :pretty

# Default value for :log_level is :debug
# set :log_level, :debug

# Default value for :pty is false
# set :pty, true

# Default value for :linked_files is []
set :linked_files, %w{db/production.sqlite3}

# Default value for linked_dirs is []
set :linked_dirs, %w{log tmp/pids tmp/cache tmp/sockets vendor/bundle public/system}

# Default value for default_env is {}
# set :default_env, { path: "/opt/ruby/bin:$PATH" }

# Default value for keep_releases is 5
# set :keep_releases, 5

# ----------------------------------------------------------------------------
# Deploy tasks
# ----------------------------------------------------------------------------

namespace :deploy do

  desc 'Restart application'
  task :restart do
    on roles(:app), in: :sequence, wait: 5 do
      execute :touch, release_path.join('tmp/restart.txt')
    end
  end

  after :publishing, :restart

  after :restart, :clear_cache do
    on roles(:app), in: :sequence, wait: 5 do
#       within release_path do
#         if test("[ -e ./tmp/pids/websocket_rails.pid ]")
#           execute :rake, 'websocket_rails:stop_server'
#         end
#         execute :rake, 'websocket_rails:start_server'
#       end
    end
  end

# ----------------------------------------------------------------------------
# Task integration
# ----------------------------------------------------------------------------

after 'deploy:restart', 'airbrake:deploy'

end
