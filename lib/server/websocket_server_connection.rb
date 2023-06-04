$connections = []

class WebsocketServerConnection
  attr_accessor :ws, :user, :get_list_of_users_was_handled

  class << self
    def by_user(user)
      $connections.find { |c| c.user == user }
    end
  end

  def initialize(env)
    @ws = Faye::WebSocket.new(env)
    @on_login = []
    dbg ["SERVER", "will add #{self.inspect} to #{$connections}"]
    $connections << self

    ws.on :open do |event|
      dbg ["SERVER", :open]
      send_message([:please_tell_me_who_you_are])
    end

    ws.on :message do |event|
      message = JSON.parse(event.data)

      dbg ["SERVER", :message_received, message]

      message[0] = message[0].to_sym

      handle_message(message)
    end

    ws.on :close do |event|
      dbg ["SERVER", :close, event.code, event.reason]
      self.ws = nil
    end
  end

  def handle_message(message)
    message.push(nil) if message.length == 1
    case message
    in :login, user
      handle_login(user)
    in :get_list_of_users, _
      handle_get_list_of_users
    in :invite_for_game, invitee
      self
        .class
        .by_user(invitee)
        .send_message([:game_invitation_from, self.user])
    in :accept_game_invitation, invitator
      send_message([:game_start_with, invitator, invitator])
      self.class.by_user(invitator).send_message([:game_start_with, self.user, invitator])
    else
      raise "message not matched: #{message.inspect}"
    end
  end

  def handle_get_list_of_users
    send_message([:users_present, other_logged_in_connections.map(&:user)])
    self.get_list_of_users_was_handled = true
  end

  def handle_login(user)
    self.user = user
    send_message([:logged_in, "Welcome, #{self.user}!"])
    tell_other_users_that_we_have_a_new_user
  end

  def on_login(&block)
    @on_login << block
  end

  def tell_other_users_that_we_have_a_new_user
    dbg ["SERVER", "tell_other_users_that_we_have_a_new_user", formatted_user]
    dbg [
          "SERVER",
          "tell_other_users_that_we_have_a_new_user",
          other_connections,
        ]
    other_connections_get_list_of_users_was_handled.each do |conn|
      conn.send_message([:user_entered, user])
    end
  end

  def send_message(message)
    dbg ["SERVER", "====> to #{formatted_user}", message]
    ws.send(message.to_json)
  end

  def formatted_user
    user || "<anonymous>"
  end

  def inspect
    "#<connection user=#{formatted_user}>"
  end

  def other_connections
    $connections.reject { |c| c == self }
  end

  def other_logged_in_connections
    other_connections.reject { |c| !c.user }
  end

  def other_connections_get_list_of_users_was_handled
    other_connections.select { |c| c.get_list_of_users_was_handled }
  end

  def close
    ws.close if ws
  end
end
