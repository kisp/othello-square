Given("that I visit the homepage") do
  visit(SERVER_HTTP_URL)
  expect(page).to have_css("h1", text: "Othello Square")
end

Given("I am asked to login with my nickname") do
  expect(page).to have_css("h2", text: "Please login with your nickname")
  expect(page).not_to have_css("#message")
end

When("I login as {string}") do |nickname|
  fill_in("Nickname", with: nickname)
  click_button("Login")
end

When('I login as {string} by hitting enter') do |nickname|
  fill_in("Nickname", with: nickname)
  find('#nickname').native.send_keys(:return)
end

Then("I see the welcome message {string}") do |message|
  expect(page).to have_css("#message", text: message)
end
