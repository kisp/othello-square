require "service_manager"

ServiceManager.define_service "game_server" do |s|
  s.start_cmd = "bundle exec rackup -Ilib"
  s.loaded_cue = %r{Listening on http:\/\/127.0.0.1:9292}
  s.cwd = Dir.pwd
  s.pid_file = "game_server.pid"
end
