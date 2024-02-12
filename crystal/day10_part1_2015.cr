
def read_input(filename : String) : String
  File.open(filename) do |file|
    file.gets_to_end
  end
end

def look_and_say(sequence : String, iterations : Int) : String
  iterations.times do
    sequence = next_sequence(sequence)
  end
  sequence
end

def next_sequence(sequence : String) : String
  result = ""
  i = 0
  while i < sequence.size
    count = 1
    digit = sequence[i]
    j = i + 1
    while j < sequence.size && sequence[j] == digit
      count += 1
      j += 1
    end
    result += "#{count}#{digit}"
    i += count
  end
  result
end

initial_sequence = read_input("input.txt")
result = look_and_say(initial_sequence, 40)
puts result.size
