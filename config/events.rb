# -*- encoding : utf-8 -*-
WebsocketRails::EventMap.describe do
  # You can use this file to map incoming events to controller actions.
  # One event can be mapped to any number of controller actions. The
  # actions will be executed in the order they were subscribed.
  #
  # Uncomment and edit the next line to handle the client connected event:
  #   subscribe :client_connected, :to => Controller, :with_method => :method_name
  #
  # Here is an example of mapping namespaced events:
  #   namespace :product do
  #     subscribe :new, :to => ProductController, :with_method => :new_product
  #   end
  # The above will handle an event triggered on the client like `product.new`.


  subscribe :client_connected, :to => SocketController, :with_method => :client_connected
  subscribe :client_disconnected, :to => SocketController, :with_method => :client_disconnected
  subscribe :simulateGrid, :to => SocketController, :with_method => :receive_code
  subscribe :stopSimulation, :to =>SocketController, :with_method => :stopSimulation

  namespace :ship do
    subscribe :left, :to => SocketController, :with_method => :rotateShipLeft
    subscribe :move, :to => SocketController, :with_method => :moveShip
    subscribe :right, :to => SocketController, :with_method => :rotateShipRight
    subscribe :put, :to => SocketController, :with_method => :put
    subscribe :take, :to => SocketController, :with_method => :take
    subscribe :look, :to => SocketController, :with_method => :look


  end
end
