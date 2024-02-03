
file = File.read("input.txt")
input = file.strip
positions = input.split("\n").map { |line| line.scan(/\d+/).map(&:to_i)[1] }

def solve(positions)
  play(positions, [0, 0], 3, true, {})
end

def play(positions, scores, rolls_left_in_turn, is_player1s_turn, memo)
  key = "#{positions}#{scores}#{rolls_left_in_turn}#{is_player1s_turn}"
  return memo[key] if memo[key]

  player_index = is_player1s_turn ? 0 : 1
  scores_copy = scores.dup

  if rolls_left_in_turn == 0
    scores_copy[player_index] += positions[player_index]

    if scores_copy[player_index] >= 21
      return player_index == 0 ? [1, 0] : [0, 1]
    end

    is_player1s_turn = !is_player1s_turn
    rolls_left_in_turn = 3
    player_index = (player_index + 1) % 2
  end

  wins1 = wins2 = 0
  (1..3).each do |roll|
    positions_copy = positions.dup
    positions_copy[player_index] += roll
    positions_copy[player_index] -= 10 if positions_copy[player_index] > 10
    r1, r2 = play(positions_copy, scores_copy, rolls_left_in_turn - 1, is_player1s_turn, memo)
    wins1 += r1
    wins2 += r2
  end

  memo[key] = [wins1, wins2]
  [wins1, wins2]
end

result = solve(positions)
puts result.max
