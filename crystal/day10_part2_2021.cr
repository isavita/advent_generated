require "file_utils"

def check_and_complete_line(line)
  pairings = {')' => '(', ']' => '[', '}' => '{', '>' => '<'}
  score_values = {')' => 1_i64, ']' => 2_i64, '}' => 3_i64, '>' => 4_i64}
  opening = "([{<"
  closing = ")]}>"
  stack = [] of Char

  line.each_char do |char|
    if opening.includes? char
      stack << char
    elsif closing.includes? char
      if stack.empty? || stack.last != pairings[char]
        return 0_i64, false
      end
      stack.pop
    end
  end

  if stack.empty?
    return 0_i64, false
  end

  score = 0_i64
  stack.reverse_each do |char|
    score *= 5_i64
    score += score_values[get_closing_char(char)]
  end
  return score, true
end

def get_closing_char(opening_char)
  case opening_char
  when '('; return ')'
  when '['; return ']'
  when '{'; return '}'
  when '<'; return '>'
  else; return ' '
  end
end

scores = [] of Int64
File.open("input.txt") do |file|
  file.each_line do |line|
    score, incomplete = check_and_complete_line(line.chomp)
    scores << score if incomplete
  end
end

scores.sort!
middle_score = scores[scores.size // 2]
puts middle_score