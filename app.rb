require "eventmachine"
require "em-synchrony"
require "faye/websocket"
require "sinatra"

require "common"
require "server/websocket_server_connection"

module Rack
  class Lint
    def call(env = nil)
      @app.call(env)
    end
  end
end

DEBUG = ENV["DEBUG"]&.downcase == "true"

get "/" do
  send_file File.join(settings.public_folder, "index.html")
end

get "/cable" do
  404 unless Faye::WebSocket.websocket?(env)

  c = WebsocketServerConnection.new(env)
  c.ws.rack_response
end

get "/reset" do
  $connections.each do |conn|
    dbg ["SERVER", "Calling conn.close", conn]
    conn.close
  end
  $connections = []
  if EM.reactor_running?
    dbg ["SERVER", "the EM reactor is running, so we will stop it"]
    EM.stop
    max_iterations = 30
    while EM.reactor_running? && max_iterations > 0
      dbg ["SERVER", "Waiting for EM reactor to stop"]
      sleep 0.1
      max_iterations = max_iterations - 1
    end
    raise "reactor is stil running" if EM.reactor_running?
  end
end
