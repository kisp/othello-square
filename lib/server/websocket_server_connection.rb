$connections = []

class WebsocketServerConnection
  attr_accessor :ws, :user, :get_list_of_users_was_handled

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

    # @on_login.each { |proc| proc.call }
    # tell_this_user_about_existing_users
    tell_other_users_that_we_have_a_new_user
  end

  # def tell_this_user_about_existing_users
  #   $connections.each do |conn|
  #     unless conn.user == user
  #       send_message([:user_entered, conn.user]) if conn.user
  #     end
  #   end
  # end

  def on_login(&block)
    @on_login << block
  end

  def tell_other_users_that_we_have_a_new_user
    # dbg [
    #       "SERVER",
    #       "doing TELL_OTHER_USERS_THAT_WE_HAVE_A_NEW_USER for",
    #       user,
    #       $connections,
    #     ]
    # message = [:user_entered, user]
    # dbg ["SERVER", "$connections is", $connections]
    # dbg ["SERVER", "DELAYED", "$connections is", $connections]
    # $connections.each do |conn|
    #   unless conn.user == user
    #     if conn.user
    #       dbg ["SERVER", "CAN SEND DIRECT", "tell #{conn.user} about #{user}"]
    #       conn.send_message(message)
    #     else
    #       dbg [
    #             "SERVER",
    #             "WILL SEND MESSAGE LATER",
    #             "tell #{conn.formatted_user} about #{user}",
    #           ]
    #       conn.on_login do
    #         dbg [
    #               "SERVER",
    #               "NOW SENDING DELAYED",
    #               "tell #{conn.user} about #{user}",
    #               message,
    #             ]
    #         conn.send_message(message)
    #       end
    #     end
    #   end
    # end
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
