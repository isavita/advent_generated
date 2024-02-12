
File.open("input.txt") do |file|
  total_score = 0
  file.each_line do |line|
    score, corrupted = check_line(line)
    total_score += score if corrupted
  end
  puts total_score
end

def check_line(line)
  pairings = {')' => '(', ']' => '[', '}' => '{', '>' => '<'}
  scores = {')' => 3, ']' => 57, '}' => 1197, '>' => 25137}
  stack = [] of Char

  line.each_char do |char|
    case char
    when '(', '[', '{', '<'
      stack.push(char)
    when ')', ']', '}', '>'
      return scores[char], true if stack.empty? || stack.last != pairings[char]
      stack.pop
    end
  end

  return 0, false
end
