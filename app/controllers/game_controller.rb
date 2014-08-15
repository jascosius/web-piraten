# -*- encoding : utf-8 -*-
class GameController < ApplicationController

  def index
    puts "Languages:"
    puts LANGUAGES
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
