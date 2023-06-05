module KnowsFrontend
  def as_user(user, &block)
    user == "I" ? block.call : Capybara.using_session(user, &block)
  end

  def visit_the_homepage
    visit(SERVER_HTTP_URL)
    expect(page).to have_css("h1", text: "Othello Square")
  end

  def asked_to_login_with_nickname
    expect(page).to have_css("h2", text: "Please login")
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
    expect(page).to have_css(".toast-message", text: message)
    expect(page).not_to have_css("h2", text: "Please login")
    expect(page).not_to have_css("input#nickname")
  end

  def can_see_that_other_user_is_there!(other_user)
    expect(page).to have_css("h2", text: "Online users")
    expect(page).to have_css("#user_#{other_user}")
  end

  def invite_for_game(invitee)
    within("#user_#{invitee}") { click_button("Invite for game") }
  end

  def game_invitation_from!(invitator)
    expect(page).to have_css("h2", text: "#{invitator} invites you for a game!")
  end

  def accept_game_invitation(invitator)
    within(".game-invitation[data-invitator=#{invitator}]") { click_button("Accept") }
  end

  def game_start_received!(other_user)
    expect(page).to have_css("#board")
  end

  def sees_the_its_your_turn_message
    expect(page).to have_css("#game_message", text: "It's your turn")
  end

  def sees_the_waiting_for_turn_message(other_user)
    expect(page).to have_css(
      "#game_message",
      text: "Waiting for #{other_user}'s turn",
    )
  end

  def move_to(square)
    page.find_by_id("square_#{square.join("")}").click
  end

  def receive_move_to(square, color)
    piece_class = color == "black" ? "bp" : "wp"
    expect(page).to have_css("#square_#{square.join("")} .#{piece_class}")
  end

  def sees_pieces_balance(balance)
    expect(page).to have_css("#board[data-pieces-balance=\"#{balance}\"]")
  end

  def show_page_html
    puts page.html
  end

  def sees_square_content(square, content)
    visible = true
    case content
    in "@"
      content_class = "bp"
    in "O"
      content_class = "wp"
    in ""
      content_class = "ee"
      visible = false
    in "+"
      content_class = "lm"
    end
    expect(page).to have_css("#square_#{square.join("")} .#{content_class}", visible: visible)
  end
end
