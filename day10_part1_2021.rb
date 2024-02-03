
def syntax_error_score(file)
  scores = { ')' => 3, ']' => 57, '}' => 1197, '>' => 25137 }
  pairs = { '(' => ')', '[' => ']', '{' => '}', '<' => '>' }
  total_score = 0

  File.readlines(file).each do |line|
    stack = []
    line.strip.each_char do |char|
      if pairs.keys.include?(char)
        stack.push(char)
      else
        if char == pairs[stack.last]
          stack.pop
        else
          total_score += scores[char]
          break
        end
      end
    end
  end

  total_score
end

puts syntax_error_score('input.txt')
