module KnowsBackend
  def as_user(user, &block)
    WebsocketConnection.by_user(user).instance_eval(&block)
  end
end
