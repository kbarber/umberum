#!/usr/bin/env ruby
# Copyright 2010 Bob.sh
#
# Main sinatra application
#

require 'rubygems'
require 'sinatra'
require 'crowd'
require 'yaml'
require 'pp'

# Load configuration
CONF = YAML.load_file("/etc/organic/desktop.conf")

# Configure rack sessions
#use Rack::Session::Memcache,	:key => CONF["rack"]["cookie_key"],
#                              :secret => CONF["rack"]["cookie_secret"]

# Prepare crowd
#Crowd.crowd_url = CONF["crowd"]["url"]
#Crowd.crowd_app_name = CONF["crowd"]["app_name"]
#Crowd.crowd_app_pword = CONF["crowd"]["app_pword"]
#Crowd.authenticate_application

# Configure application differently for production use
configure :production do
  set :show_exceptions, false
  # TODO: instead of exceptions, show a nicer error message.
end

before do

  # First lets try to establish any prior SSO authentication using crowd.token_key
  # and looking at our existing sessions.

  #crowd_token = request.cookies["crowd.token_key"]

  # If a crowd token is set, and either the username isn't set or 
  # the cookie crowd.token_key doesn't match the session crowd_token
  # then lets do a revalidation
#  if crowd_token and (
#      !session.key?(:username) or
#      !session.key?(:crowd_token) or
#      (crowd_token != session[:crowd_token])
#    ) then
#
#    # Find principal and check if its real
#    crowd_principal = Crowd.find_principal_by_token(crowd_token)
#    if crowd_principal == nil then
#      # No such principal, or its expired. Clear the cookie and redirect.
#      response.delete_cookie("crowd.token_key", {
#        :domain => CONF["crowd"]["domain"],
#        :path => "/" })
#      redirect '/login'
#    end
#     
#    # So lets set the session up properly
#    session[:username] = crowd_principal["name"]
#    session[:crowd_token] = crowd_token
#  end
#
#  # Now lets apply any filtering.
#
#  # Check if the session is authenticated (ie. username is set) otherwise send
#  # the user back to /login.
#  if !session.key?(:username) and request.path_info !~ /^\/(log(out|in)|register)/ then
#    redirect '/login'
#  end
end

get '/' do
  redirect '/desktop.html'
end


# vim: ts=2 sw=2 expandtab:
