When('{string} makes a move to {string}') do |user, square|
  as_user(user) { move_to(square.split(",").map(&:to_i)) }
end

Then('{string} sees a {string} move to {string} from his/her opponent') do |user, color, square|
    as_user(user) { receive_move_to(square.split(",").map(&:to_i), color) }
end

Then('{string} sees a pieces balance of {string}') do |user, balance|
  as_user(user) { sees_pieces_balance(balance) }
end
