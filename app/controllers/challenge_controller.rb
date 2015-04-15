# -*- encoding : utf-8 -*-
# load requested help-file and send it to the view
class ChallengeController < ApplicationController

  def index
    unless params[:file]
      redirect_to :controller => :challenge, :action => :index, :file => :index
      return
    end
    filename = params[:file]
    content = ''
    begin
      File.open("challenge/#{filename}.md") do |file|
        content = file.read
      end
    rescue
      redirect_to :controller => :challenge, :action => :index, :file => :index
      return
    else
      @code = Markdown.new(content).to_html
    end
  end

end

