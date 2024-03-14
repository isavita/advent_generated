def check_and_complete_line(line)
  pairings = {')' => '(', ']' => '[', '}' => '{', '>' => '<'}
  score_values = {')' => 1, ']' => 2, '}' => 3, '>' => 4}
  opening = "([{<"
  closing = ")]}>"
  stack = []

  line.each_char do |char|
    if opening.include?(char)
      stack.push(char)
    elsif closing.include?(char)
      if stack.empty? || stack.last != pairings[char]
        return [0, false] # corrupted line
      end
      stack.pop # pop from stack
    end
  end

  if stack.empty?
    return [0, false] # line is not incomplete
  end

  # Calculate score for incomplete line
  score = 0
  stack.reverse.each do |opening_char|
    score = score * 5 + score_values[get_closing_char(opening_char)]
  end
  [score, true]
end

def get_closing_char(opening_char)
  case opening_char
  when '('
    ')'
  when '['
    ']'
  when '{'
    '}'
  when '<'
    '>'
  else
    ' '
  end
end

scores = []
File.readlines('input.txt').each do |line|
  score, incomplete = check_and_complete_line(line.chomp)
  scores.push(score) if incomplete
end

scores.sort!
middle_score = scores[scores.length / 2]
puts middle_score