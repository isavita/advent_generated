
def parse_input(input_str : String) : Array(Int32)
  player1_pos = 0
  player2_pos = 0
  input_str.each_line do |line|
    if line.starts_with?("Player 1")
      player1_pos = line.split(": ").last.to_i32
    elsif line.starts_with?("Player 2")
      player2_pos = line.split(": ").last.to_i32
    end
  end
  [player1_pos, player2_pos]
end

def play(positions : Array(Int32), scores : Array(Int32), rolls_left : Int32,
         is_player1_turn : Bool, memo : Hash(String, Tuple(Int64, Int64))) : Tuple(Int64, Int64)
  key = "#{positions.join(',')}|#{scores.join(',')}|#{rolls_left}|#{is_player1_turn}"
  if memo.has_key?(key)
    return memo[key]
  end

  player_index = is_player1_turn ? 0 : 1
  scores_copy = scores.dup

  if rolls_left == 0
    scores_copy[player_index] += positions[player_index]
    if scores_copy[player_index] >= 21
      result = player_index == 0 ? {1_i64, 0_i64} : {0_i64, 1_i64}
      memo[key] = result
      return result
    end
    is_player1_turn = !is_player1_turn
    rolls_left = 3
    player_index = (player_index + 1) % 2
  end

  wins1 = 0_i64
  wins2 = 0_i64

  (1..3).each do |roll|
    positions_copy = positions.dup
    positions_copy[player_index] = (positions_copy[player_index] + roll - 1) % 10 + 1
    r1, r2 = play(positions_copy, scores_copy, rolls_left - 1, is_player1_turn, memo)
    wins1 += r1
    wins2 += r2
  end

  result = {wins1, wins2}
  memo[key] = result
  result
end

def solve(input_str : String) : Int64
  positions = parse_input(input_str)
  memo = Hash(String, Tuple(Int64, Int64)).new
  wins1, wins2 = play(positions, [0_i32, 0_i32], 3_i32, true, memo)
  wins1 > wins2 ? wins1 : wins2
end

input_str = File.read("input.txt").strip
puts solve(input_str)
