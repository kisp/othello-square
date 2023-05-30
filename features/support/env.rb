require "service_manager"
require "capybara/cucumber"

require_relative "./service_manager_patch"

require "net/http"

ServiceManager.start

SERVER_HTTP_URL = "http://localhost:9292/"
