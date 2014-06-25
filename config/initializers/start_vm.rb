if Rails.env.development?
  Thread.start do
    puts 'Started virtual machine for simulation (development only)'
    # blocking the thread
    system('ruby ./app/vm/vm.rb')
    puts 'Stopped virtual machine for simulation'
  end
end