if Rails.env.development?
  Thread.start do
    puts 'Started virtual machine for simulation (development only)'
    # blocking the thread
    # system('ruby -C vm/vm vm.rb development')
  end
end