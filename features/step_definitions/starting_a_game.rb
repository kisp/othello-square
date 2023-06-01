When("{string} invites {string} to play a game") do |invitator, invitee|
  as_user(invitator) { invite_for_game(invitee) }
end

Then(
  "{string} receives a game invitation from {string}",
) { |invitee, invitator| as_user(invitee) { game_invitation_from!(invitator) } }

When(
  "{string} accepts the game invitation from {string}",
) do |invitee, invitator|
  as_user(invitee) { accept_game_invitation(invitator) }
end

Then("{string} gets a game starts with {string} message") do |user, other_user|
  as_user(user) { game_start_received!(other_user) }
end
