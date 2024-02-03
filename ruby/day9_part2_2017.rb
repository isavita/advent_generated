
def process_stream(stream)
  score = 0
  depth = 0
  garbage = false
  ignore_next = false
  garbage_count = 0

  stream.each_char do |char|
    if ignore_next
      ignore_next = false
      next
    end

    if garbage
      case char
      when '>'
        garbage = false
      when '!'
        ignore_next = true
      else
        garbage_count += 1
      end
    else
      case char
      when '{'
        depth += 1
        score += depth
      when '}'
        depth -= 1
      when '<'
        garbage = true
      when '!'
        ignore_next = true
      end
    end
  end

  [score, garbage_count]
end

input = File.read('input.txt')
score, garbage_count = process_stream(input)
puts "Score: #{score}, Garbage Count: #{garbage_count}"
