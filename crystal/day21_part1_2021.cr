
data = File.read("input.txt").strip.split("\n")
player1_start = data[0][28..].strip.to_i
player2_start = data[1][28..].strip.to_i
player1_pos = player1_start
player2_pos = player2_start
player1_score = 0
player2_score = 0
die_roll = 1
roll_count = 0

loop do
  # Player 1
  rolls = die_roll % 100 + (die_roll + 1) % 100 + (die_roll + 2) % 100
  roll_count += 3
  die_roll += 3
  player1_pos = (player1_pos + rolls - 1) % 10 + 1
  player1_score += player1_pos

  if player1_score >= 1000
    puts player2_score * roll_count
    break
  end

  # Player 2
  rolls = die_roll % 100 + (die_roll + 1) % 100 + (die_roll + 2) % 100
  roll_count += 3
  die_roll += 3
  player2_pos = (player2_pos + rolls - 1) % 10 + 1
  player2_score += player2_pos

  if player2_score >= 1000
    puts player1_score * roll_count
    break
  end
end
