
file = File.open("input.txt", "r")
total_score = 0

file.each_line do |line|
  opponent, your_move = line[0], line[2]
  
  score = 0
  if your_move == 'X'
    score = 1
  elsif your_move == 'Y'
    score = 2
  elsif your_move == 'Z'
    score = 3
  end

  if (opponent == 'A' && your_move == 'Y') || (opponent == 'B' && your_move == 'Z') || (opponent == 'C' && your_move == 'X')
    score += 6
  elsif opponent == 'A' && your_move == 'X' || opponent == 'B' && your_move == 'Y' || opponent == 'C' && your_move == 'Z'
    score += 3
  end

  total_score += score
end

puts total_score
