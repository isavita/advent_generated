file = File.open("input.txt", "r")
input = file.gets_to_end.chomp
file.close

scoreboard = [3, 7]
elf1, elf2 = 0, 1
input_sequence = input.chars.map(&.to_i)

loop do
  new_score = scoreboard[elf1] + scoreboard[elf2]
  if new_score >= 10
    scoreboard << new_score // 10
    if check_sequence(scoreboard, input_sequence)
      puts scoreboard.size - input_sequence.size
      exit
    end
  end
  scoreboard << new_score % 10
  if check_sequence(scoreboard, input_sequence)
    puts scoreboard.size - input_sequence.size
    exit
  end

  elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.size
  elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.size
end

def check_sequence(scoreboard, sequence)
  return false if scoreboard.size < sequence.size
  start = scoreboard.size - sequence.size
  sequence.each_with_index do |v, i|
    return false if scoreboard[start + i] != v
  end
  true
end