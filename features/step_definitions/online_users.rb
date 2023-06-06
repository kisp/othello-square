Then("I can see that {string} is there") do |other_user|
  can_see_that_other_user_is_there!(other_user)
end

Then("{string} can see that {string} is there") do |user, other_user|
  as_user(user) { can_see_that_other_user_is_there!(other_user) }
end

Given('{user} can see that {user} is currently not playing') do |user, other_user|
    as_user(user) { can_see_that_other_user_is_currently_not_playing!(other_user) }
end

Then('{user} can see that {user} is currently playing with {user}') do |user, other_user, partner|
    as_user(user) { can_see_that_other_user_is_currently_playing!(other_user, partner) }
end
