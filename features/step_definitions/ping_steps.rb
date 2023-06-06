When("{string} sends a ping message") { |user| as_user(user) { send_ping } }

Then("{string} receives a pong message") do |user|
  as_user(user) { pong_received! }
end
