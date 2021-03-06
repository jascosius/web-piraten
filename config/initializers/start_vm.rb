# -*- encoding : utf-8 -*-
# Start the VM-Script when running in development-mode
if ENV['WITHOUT_VM']
  puts 'WITHOUT_VM environment variable is set, start vm manually.'
elsif Rails.env.development?
  Thread.start do
    puts 'Started virtual machine for simulation (development only)'
    # blocking the thread
    system('ruby -C vm/vm vm.rb development')
  end
else
  puts 'VM is not started.'
end
