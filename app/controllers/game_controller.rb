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

    id = params[:name].downcase.to_sym
    language = LANGUAGES[id]
    if language == nil
      flash[:error] = "unbekannte Sprache '#{id}'"
      redirect_to :action => :index
      return
    end
    name = language.name
    scripts = language.scripts
    options = language.options
    @scripts = scripts
    @language = {:name => name, :scripts => scripts, :options => options}.to_json
  end

  def show
  end

  def new
  end
end
