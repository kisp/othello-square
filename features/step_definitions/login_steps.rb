When("I login as {string}") { |nickname| login_as(nickname) }

When("{user} logs in with his/her username/nickname") do |nickname|
  as_user(nickname) { login_as(nickname) }
end

When("I login as {string} by hitting enter") do |nickname|
  login_as_by_hitting_enter(nickname)
end
