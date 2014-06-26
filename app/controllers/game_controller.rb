# -*- encoding : utf-8 -*-
class GameController < ApplicationController
  def index
    @language = ['ruby','java']
  end

  def learn
    @language = params[:name]
  end
  def show
  end
  def new

  end
end
