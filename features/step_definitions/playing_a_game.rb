When("{string} makes a move to {string}") do |user, square|
  as_user(user) { move_to(square.split(",").map(&:to_i)) }
end

Then(
  "{string} sees a {string} move to {string} from his/her opponent",
) do |user, color, square|
  as_user(user) { receive_move_to(square.split(",").map(&:to_i), color) }
end

Then("{string} sees a pieces balance of {string}") do |user, balance|
  as_user(user) { sees_pieces_balance(balance) }
end

def board_content(board)
  Enumerator.new do |y|
    board.raw[1..].each { |row| row[1..].each { |square| y << square } }
  end
end

def board_squares
  Enumerator.new do |y|
    (1..8).each { |row| (1..8).each { |col| y << [row, col] } }
  end
end

def board_count(board, pattern)
  board_content(board).grep(pattern).count
end

def board_balance(board)
  [board_count(board, "@"), board_count(board, "O")].join("/")
end

Given("{string} sees the board") do |user, board|
  balance = board_balance(board)
  as_user(user) do
    sees_pieces_balance(balance)
    board_content(board)
      .zip(board_squares)
      .each { |content, square| sees_square_content(square, content) }
  end
end
