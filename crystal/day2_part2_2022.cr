
file = File.open("input.txt")
total_score = 0

file.each_line do |line|
  opponent, round_end = line[0], line[2]

  your_move = ' '
  if round_end == 'X'
    your_move = opponent == 'A' ? 'Z' : opponent == 'B' ? 'X' : 'Y'
  elsif round_end == 'Y'
    your_move = opponent == 'A' ? 'X' : opponent == 'B' ? 'Y' : 'Z'
  else
    your_move = opponent == 'A' ? 'Y' : opponent == 'B' ? 'Z' : 'X'
  end

  score = 0
  case your_move
  when 'X'
    score = 1
  when 'Y'
    score = 2
  when 'Z'
    score = 3
  end

  if (opponent == 'A' && your_move == 'Y') || (opponent == 'B' && your_move == 'Z') || (opponent == 'C' && your_move == 'X')
    score += 6
  elsif (opponent == 'A' && your_move == 'X') || (opponent == 'B' && your_move == 'Y') || (opponent == 'C' && your_move == 'Z')
    score += 3
  end

  total_score += score
end

puts total_score
