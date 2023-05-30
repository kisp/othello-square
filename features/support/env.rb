require "eventmachine"
require "em-synchrony"
require "faye/websocket"
require "service_manager"
require "capybara/cucumber"

require "common"
require "ascii_colors"
require_relative "./service_manager_patch"

require "net/http"

ServiceManager.start

SERVER_WS_URL = "ws://localhost:9292/cable"

SERVER_HTTP_URL = "http://localhost:9292/"
SERVER_HTTP_RESET_URL = "http://localhost:9292/reset"

DEBUG = ENV["DEBUG"]&.downcase == "true"

Around("@em") do |scenario, block|
  EM.synchrony do
    block.call
    WebsocketConnection.close_all
    EM.stop
  end
end
