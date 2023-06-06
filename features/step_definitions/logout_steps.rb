When('{user} logs out') do |nickname|
  as_user(nickname) { logout_as(nickname) }
end
