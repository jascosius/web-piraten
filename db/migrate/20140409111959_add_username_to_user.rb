# -*- encoding : utf-8 -*-
class AddUsernameToUser < ActiveRecord::Migration
  def change
    add_column :users, :username, :string, {null: false, default: "", limit: 16}
    add_index :users, :username, unique: true
  end
end
