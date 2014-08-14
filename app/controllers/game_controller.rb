# -*- encoding : utf-8 -*-
class GameController < ApplicationController

  def index
    @language = LANGUAGES.keys
  end

  def learn
    language = LANGUAGES[[params[:name]][0].to_sym]
    name = language.name
    scripts = language.scripts
    options = language.options
    @scripts = scripts
    @language = {:name => name, :scripts => scripts, :options => options}.to_json
    @language

  end

  def show
  end

  def new
  end
end
