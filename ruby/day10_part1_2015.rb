
def look_and_say(sequence, iterations)
  iterations.times do
    new_seq = ""
    count = 1
    prev_char = sequence[0]
    
    (1...sequence.length).each do |i|
      if sequence[i] == prev_char
        count += 1
      else
        new_seq << count.to_s << prev_char
        count = 1
        prev_char = sequence[i]
      end
    end
    
    new_seq << count.to_s << prev_char
    sequence = new_seq
  end
  sequence
end

initial_sequence = File.read("input.txt").strip
result = look_and_say(initial_sequence, 40)
puts result.length
