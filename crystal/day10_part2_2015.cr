file = File.open("input.txt")
initial_sequence = file.gets_to_end.chomp

result = look_and_say(initial_sequence, 50)
puts result.size

def look_and_say(sequence, iterations)
  iterations.times { sequence = next_sequence(sequence) }
  sequence
end

def next_sequence(sequence)
  result = String::Builder.new
  i = 0
  while i < sequence.size
    count = 1
    digit = sequence[i]
    while i + 1 < sequence.size && sequence[i + 1] == digit
      i += 1
      count += 1
    end
    result << count.to_s << digit
    i += 1
  end
  result.to_s
end