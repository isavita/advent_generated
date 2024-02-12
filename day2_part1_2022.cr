
file = File.open("input.txt")
total_score = 0

file.each_line do |line|
  opponent, your_move = line[0], line[2]
  
  score = case your_move
  when 'X'
    1
  when 'Y'
    2
  when 'Z'
    3
  else
    0
  end

  if (opponent == 'A' && your_move == 'Y') || (opponent == 'B' && your_move == 'Z') || (opponent == 'C' && your_move == 'X')
    score += 6
  elsif (opponent == 'A' && your_move == 'X') || (opponent == 'B' && your_move == 'Y') || (opponent == 'C' && your_move == 'Z')
    score += 3
  end

  total_score += score
end

puts total_score
