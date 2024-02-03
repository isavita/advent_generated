
input = File.read('input.txt')
score = 0
depth = 0
in_garbage = false
skip = false

input.each_char do |char|
  if skip
    skip = false
    next
  end

  if in_garbage
    if char == '>'
      in_garbage = false
    elsif char == '!'
      skip = true
    end
  else
    case char
    when '{'
      depth += 1
    when '}'
      score += depth
      depth -= 1
    when '<'
      in_garbage = true
    end
  end
end

puts score
