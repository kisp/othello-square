When('{string} sends a ping message') do |user|
  as_user(user) { send_ping }
end

Then('{string} receives a pong message') do |user|
  as_user(user) { pong_received! }
end
