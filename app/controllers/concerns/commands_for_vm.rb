module CommandsForVm

  default_functions = {:to_file => {:filename => nil, :permissions => 0744, :content => nil}}

  def proof_hash(hash)
    hash
  end
end