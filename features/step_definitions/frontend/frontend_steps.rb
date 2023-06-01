module LoginHelpers
  def visit_the_homepage
    visit(SERVER_HTTP_URL)
    expect(page).to have_css("h1", text: "Othello Square")
  end

  def asked_to_login_with_nickname
    expect(page).to have_css("h2", text: "Please login with your nickname")
    expect(page).not_to have_css("#message")
  end

  def login_as(nickname)
    fill_in("Nickname", with: nickname)
    click_button("Login")
  end

  def login_as_by_hitting_enter(nickname)
    fill_in("Nickname", with: nickname)
    find("#nickname").native.send_keys(:return)
  end

  def sees_the_welcome_message(message)
    expect(page).to have_css("#message", text: message)
    expect(page).not_to have_css("h2", text: "Please login with your nickname")
    expect(page).not_to have_css("input#nickname")
  end

  def can_see_that_user_is_there(other_user)
    expect(page).to have_css("h2", text: "Online users")
    expect(page).to have_css("#user_#{other_user}")
  end
end

World(LoginHelpers)

Given("that I visit the homepage") { visit_the_homepage }
Given("that {string} visits the homepage") do |session_name|
  Capybara.using_session(session_name) { visit_the_homepage }
end

Given("I am asked to login with my nickname") { asked_to_login_with_nickname }
Given("{string} is asked to login with his/her nickname") do |session_name|
  Capybara.using_session(session_name) { asked_to_login_with_nickname }
end

When("I login as {string}") { |nickname| login_as(nickname) }
When("{string} logs in as {string}") do |session_name, nickname|
  Capybara.using_session(session_name) { login_as(nickname) }
end

When("I login as {string} by hitting enter") do |nickname|
  login_as_by_hitting_enter(nickname)
end

Then("I see the welcome message {string}") do |message|
  sees_the_welcome_message(message)
end
Then("{string} sees the welcome message {string}") do |session_name, message|
  Capybara.using_session(session_name) { sees_the_welcome_message(message) }
end

Then("I can see that {string} is there") do |other_user|
  can_see_that_user_is_there(other_user)
end
Then(
  "frontend {string} can see that {string} is there",
) do |session_name, other_user|
  Capybara.using_session(session_name) do
    can_see_that_user_is_there(other_user)
  end
end
