require 'mina/bundler'
require 'mina/rails'
require 'mina/git'
# require 'mina/rbenv'  # for rbenv support. (http://rbenv.org)
require 'mina/rvm' # for rvm support. (http://rvm.io)

# Basic settings:
#   domain       - The hostname to SSH to.
#   deploy_to    - Path to deploy into.
#   repository   - Git repo to clone from. (needed by mina/git)
#   branch       - Branch name to deploy. (needed by mina/git)
# chevalblanc:12343
set :domain, 'chevalblanc.informatik.uni-kiel.de'
set :user, 'captain'
set :port, '12343'
# set :identity_file, 'key.pem'
set :deploy_to, '/var/www/webpiraten'
set :repository, 'ssh://git@git-ps.informatik.uni-kiel.de:55055/projekte/web-piraten.git'
set :branch, 'production'

# Manually create these paths in shared/ (eg: shared/config/database.yml) in your server.
# They will be linked in the 'deploy:link_shared_paths' step.
set :shared_paths, ['log']

set :rvm_path, '/home/captain/.rvm/scripts/rvm'
# Optional settings:
#   set :user, 'foobar'    # Username in the server to SSH to.
#   set :port, '30000'     # SSH port number.

# This task is the environment that is loaded for most commands, such as
# `mina deploy` or `mina rake`.
task :environment do
  # If you're using rbenv, use this to load the rbenv environment.
  # Be sure to commit your .rbenv-version to your repository.
  # invoke :'rbenv:load'

  # For those using RVM, use this to load an RVM version@gemset.
  invoke :'rvm:use[ruby-2.1.2@web-piraten]'
end

# Put any custom mkdir's in here for when `mina setup` is ran.
# For Rails apps, we'll make some of the shared paths that are shared between
# all releases.
task :setup => :environment do
  queue! %[mkdir -p "#{deploy_to}/shared/log"]
  queue! %[chmod g+rx,u+rwx "#{deploy_to}/shared/log"]

  queue! %[mkdir -p "#{deploy_to}/shared/config"]
  queue! %[chmod g+rx,u+rwx "#{deploy_to}/shared/config"]

  queue! %[touch "#{deploy_to}/shared/config/database.yml"]
  queue %[echo "-----> Be sure to edit 'shared/config/database.yml'."]
end

desc "Deploys the current version to the server."
task :deploy => :environment do
  deploy do
    # Put things that will set up an empty directory into a fully set-up
    # instance of your project.
    invoke :'git:clone'
    invoke :'deploy:link_shared_paths'
    invoke :'bundle:install'
    # invoke :'rails:db_migrate' # we don't use a database
    invoke :'rails:assets_precompile'

    to :launch do
      queue "chmod +x #{deploy_to}/current/scripts/shutdown_server.sh"
      queue "cd #{deploy_to}/current/scripts/ && ./shutdown_server.sh"
      queue "cd #{deploy_to}/current/ && /home/captain/bin/bundle exec thin start -d -e production -p 3000"
    end
  end
end

task :restart do
  queue "cd #{deploy_to}/current/scripts/ && ./shutdown_server.sh"
  queue "cd #{deploy_to}/current/ && /home/captain/bin/bundle exec thin start -d -e production -p 3000"
end

task :start do
  queue "cd #{deploy_to}/current/ && /home/captain/bin/bundle exec thin start -d -e production -p 3000"
end

task :stop do
  queue "cd #{deploy_to}/current/scripts/ && ./shutdown_server.sh"
end

task :kill do
  queue "pkill -9 ruby || true"
end

task :deploy_vm do
  queue "touch #{deploy_to}/deploy"
  queue 'echo in progress ...'
  queue 'sleep 80'
  queue "cd #{deploy_to}/current/scripts/ && ./update_vm.sh"
  queue 'echo wait 60 seconds to complete'
end

task :deploy_vm_sh_only do
  queue "cd #{deploy_to}/current/scripts/ && ./update_vm.sh"
end

# For help in making your deploy script, see the Mina documentation:
#
#  - http://nadarei.co/mina
#  - http://nadarei.co/mina/tasks
#  - http://nadarei.co/mina/settings
#  - http://nadarei.co/mina/helpers

