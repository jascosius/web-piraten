# -*- encoding : utf-8 -*-
# initialize learning-page
class GameController < ApplicationController

  def index
    @language = LANGUAGES
  end

  def learn
    unless params.has_key? :name
      redirect_to :action => :index
      return
    end

    @id = params[:name].downcase.to_sym
    @language = LANGUAGES[@id]
    if @language == nil
      flash[:error] = "unbekannte Sprache '#{@id}'"
      redirect_to :action => :index
      return
    end

  end

  def show
  end

  def new
  end
end
