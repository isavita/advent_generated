def main
  input = File.read("input.txt").strip
  scoreboard = [3, 7]
  elf1, elf2 = 0, 1
  input_sequence = input.chars.map(&:to_i)

  loop do
    new_score = scoreboard[elf1] + scoreboard[elf2]
    if new_score >= 10
      scoreboard << new_score / 10
      break if check_sequence(scoreboard, input_sequence)
    end
    scoreboard << new_score % 10
    break if check_sequence(scoreboard, input_sequence)

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.size
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.size
  end

  puts scoreboard.size - input_sequence.size
end

def check_sequence(scoreboard, sequence)
  return false if scoreboard.size < sequence.size

  start = scoreboard.size - sequence.size
  sequence.each_with_index.all? { |value, index| scoreboard[start + index] == value }
end

main