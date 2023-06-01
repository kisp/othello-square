require "rspec/expectations"

$connections = {}

class WebsocketConnection
  include RSpec::Matchers

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
    ws.send(message.to_json)
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
    send_message([:get_list_of_users])
    wait_until(
      timeout: 1,
      message: "Waiting for other_users to be not nil",
      debug: true,
    ) { other_users }
    self.last_message = nil
  end

  def invite_for_game(invitee)
    send_message([:invite_for_game, invitee])
  end

  def game_invitation_from?(invitator)
    wait_until(
      timeout: 1,
      message: "#{user} wants to get an invitation from #{invitator}",
      interval: 0.1,
      debug: true,
      timeout_expectation:
        lambda do
          expect(last_message).to eq([:game_invitation_from, invitator])
        end,
    ) { last_message == [:game_invitation_from, invitator] }

    self.last_message = nil
    true
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
      message = JSON.parse(event.data)
      dbg_user [:message_received, message]
      message[0] = message[0].to_sym
      message.push(nil) if message.length == 1

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
        in :please_tell_me_who_you_are, _
          nothing_to_do
        in :logged_in, _
          self.logged_in = true
        in :users_present, users
          self.other_users = users
        in :game_invitation_from, _invitator
          nothing_to_do
        end
      end
    end

    wait_until(
      timeout: 3,
      message: "Waiting for open WebsocketConnection for #{user}",
    ) { open }

    prompt = [:please_tell_me_who_you_are, nil]
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
