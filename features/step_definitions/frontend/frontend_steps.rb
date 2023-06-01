Given("that {user} visit/visits the homepage") do |session_name|
  as_user(session_name) { visit_the_homepage }
end

Given("I am asked to login with my nickname") { asked_to_login_with_nickname }
Given("{string} is asked to login with his/her nickname") do |session_name|
  as_user(session_name) { asked_to_login_with_nickname }
end

Then("I see the welcome message {string}") do |message|
  sees_the_welcome_message(message)
end
Then("{string} sees the welcome message {string}") do |session_name, message|
  as_user(session_name) { sees_the_welcome_message(message) }
end
