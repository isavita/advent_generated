
file = File.open("input.txt")
score = 0
depth = 0
in_garbage = false
cancel_next = false
garbage_count = 0

file.each_line do |line|
  line.each_char do |ch|
    if cancel_next
      cancel_next = false
      next
    end

    if in_garbage
      if ch == '!'
        cancel_next = true
      elsif ch == '>'
        in_garbage = false
      else
        garbage_count += 1
      end
    else
      case ch
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
end

puts garbage_count
