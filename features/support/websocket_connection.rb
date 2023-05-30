$connections = {}

class WebsocketConnection
  attr_accessor :open, :logged_in, :last_message, :other_users
  attr_reader :ws, :user

  class << self
    def by_user(user)
      $connections[user]
    end

    def close_all
      $connections.each_value { |c| c.ws.close }
    end
  end

  def initialize(user, do_login: true)
    $connections[user] = self

    @user = user
    @other_users = nil

    connect

    login if do_login
  end

  def send_message(message)
    dbg_user ["====>", message]
    ws.send(Marshal.dump(message))
  end

  def login
    send_message([:login, user])

    # self.logged_in = true
    wait_until(timeout: 0.5, message: "Waiting for :logged_in", debug: true) do
      logged_in
    end
    self.last_message = nil
    wait_until(timeout: 3, message: "Waiting for login of #{user}") do
      logged_in
    end
  end

  def get_list_of_users
    send_message(%i[get_list_of_users dummy])
    wait_until(
      timeout: 1,
      message: "Waiting for other_users to be not nil",
      debug: true,
    ) { other_users }
    self.last_message = nil
  end

  def inspect
    "#<connection user=#{user}>"
  end

  private

  def connect
    dbg_user [:connect]
    @ws = Faye::WebSocket::Client.new(SERVER_WS_URL)

    ws.on :open do |event|
      self.open = true
    end

    ws.on :message do |event|
      message = Marshal.restore(event.data)
      dbg_user [:message_received, message]

      # these messages do not worry about last_message
      message_handled = true
      case message
      in :user_entered, user
        other_users << user
      else
        message_handled = false
      end

      unless message_handled
        if last_message
          raise "#{self.inspect} new message (#{message.inspect}), but last_message is: #{last_message.inspect}"
        end
        self.last_message = message

        case message
        in :please_tell_me_who_you_are, :dummy
          nothing_to_do
        in :logged_in, :dummy
          self.logged_in = true
        in :users_present, users
          self.other_users = users
        end
      end
    end

    wait_until(
      timeout: 3,
      message: "Waiting for open WebsocketConnection for #{user}",
    ) { open }

    prompt = %i[please_tell_me_who_you_are dummy]
    wait_until(timeout: 3, message: "Waiting for server message (#{prompt})") do
      last_message == prompt
    end
    self.last_message = nil
  end

  def dbg_user(x)
    if !user
      dbg(x)
    else
      message = x.clone.prepend(user)
      case user
      in "jane"
        dbg(message) { |s| s.black.bg_green }
      in "bob"
        dbg(message) { |s| s.black.bg_blue }
      else
        dbg(message)
      end
    end
  end

  def nothing_to_do; end
end
