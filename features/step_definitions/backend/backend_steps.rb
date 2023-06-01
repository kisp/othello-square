Given("no users are connected to the game server") do
  res = Net::HTTP.get_response(URI(SERVER_HTTP_RESET_URL))
  expect(res).to be_a Net::HTTPOK
end

Given("{string} is connected and logged in to the game server") do |user|
  WebsocketConnection.new(user)
end

Given("{string} has opened a connection to the game server") do |user|
  WebsocketConnection.new(user, do_login: false)
end

Given("{string} logs in with his/her username") do |user|
  WebsocketConnection.by_user(user).login
end

When("{string} asks the server to get a list of users") do |user|
  WebsocketConnection.by_user(user).get_list_of_users
end

Then("{string} can see that {string} is there") do |user, other_user|
  conn = WebsocketConnection.by_user(user)
  expect(conn).to be_truthy

  wait_until(
    timeout: 1,
    message: "#{user} wants to see #{other_user}",
    interval: 0.1,
    debug: true,
    timeout_expectation:
      lambda { expect(conn.other_users).to include(other_user) },
  ) { conn.other_users.include?(other_user) }
end

When("{string} invites {string} to play a game") do |invitator, invitee|
  WebsocketConnection.by_user(invitator).invite_for_game(invitee)
end

Then(
  "{string} receives a game invitation from {string}",
) do |invitee, invitator|
  conn = WebsocketConnection.by_user(invitee)
  expect(conn.game_invitation_from?(invitator)).to be_truthy
end

When(
  "{string} accepts the game invitation from {string}",
) do |invitee, invitator|
  WebsocketConnection.by_user(invitee).accept_game_invitation(invitator)
end

Then("{string} gets a game starts with {string} message") do |user, other_user|
  conn = WebsocketConnection.by_user(user)
  expect(conn.game_start_with?(other_user)).to be_truthy
end

Then("sleep for {int} seconds") { |int| mysleep(int) }

Then("just print {string} to the debug console") { |string| puts string }
