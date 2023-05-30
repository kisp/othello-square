Given('that I visit the homepage') do
  visit("https://othello-square.fly.dev")
  expect(page).to have_css('h1', text: 'Othello Square')
end

Given('I am asked to login with my nickname') do
  expect(page).to have_css('h2', text: 'Please login with your nickname')
end

When('I login as {string}') do |nickname|
  fill_in("Nickname", with: nickname)
  click_button("Login")
end

Then('I see the welcome message {string}') do |string|
  expect(page).to have_css('#message', text: 'Welcome, Peter!')
end
