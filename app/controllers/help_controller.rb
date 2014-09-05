# -*- encoding : utf-8 -*-
class HelpController < ApplicationController

  def index
    unless params[:file]
      redirect_to :controller => :help, :action => :index, :file => :index
      return
    end
    filename = params[:file]
    content = ''
    begin
      File.open("help/#{filename}.md") do |file|
        content = file.read
      end
    rescue
      redirect_to :controller => :help, :action => :index, :file => :index
      return
    else
      @code = Markdown.new(content).to_html
    end
  end

end

