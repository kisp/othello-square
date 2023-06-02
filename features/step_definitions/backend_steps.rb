Given("{string} is connected and logged in to the game server") do |user|
  WebsocketConnection.new(user)
end

Given("{string} has opened a connection to the game server") do |user|
  WebsocketConnection.new(user, do_login: false)
end

When("{string} asks the server to get a list of users") do |user|
  as_user(user) { get_list_of_users }
end

# Then("sleep for {int} seconds") { |int| mysleep(int) }

# Then("just print {string} to the debug console") { |string| puts string }
