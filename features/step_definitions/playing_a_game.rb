When('{string} makes a move to {string}') do |user, square|
  as_user(user) { move_to(square.split(",").map(&:to_i)) }
end

Then('{string} sees a move to {string} from his/her opponent') do |user, square|
    as_user(user) { receive_move_to(square.split(",").map(&:to_i)) }
end
