Then("I can see that {string} is there") do |other_user|
  can_see_that_other_user_is_there!(other_user)
end

Then("{string} can see that {string} is there") do |user, other_user|
  as_user(user) { can_see_that_other_user_is_there!(other_user) }
end
