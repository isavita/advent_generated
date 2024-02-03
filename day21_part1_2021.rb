
input = File.readlines("input.txt").map(&:chomp)

player1_start = input[0].split(": ")[1].to_i
player2_start = input[1].split(": ")[1].to_i

player1_score = 0
player2_score = 0
die_roll = 1
total_rolls = 0

while player1_score < 1000 && player2_score < 1000
  roll_sum = die_roll + (die_roll + 1) + (die_roll + 2)
  player1_start = (player1_start + roll_sum - 1) % 10 + 1
  player1_score += player1_start

  die_roll = (die_roll + 3) % 100

  roll_sum = die_roll + (die_roll + 1) + (die_roll + 2)
  player2_start = (player2_start + roll_sum - 1) % 10 + 1
  player2_score += player2_start

  die_roll = (die_roll + 3) % 100

  total_rolls += 6
end

if player1_score >= 1000
  puts player2_score * total_rolls
else
  puts player1_score * total_rolls
end
