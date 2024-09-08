def solve_chocolate_charts(input)
  scoreboard = [3, 7]
  elf1, elf2 = 0, 1

  while scoreboard.size < input + 10
    sum = scoreboard[elf1] + scoreboard[elf2]
    scoreboard.push(sum / 10) if sum >= 10
    scoreboard.push(sum % 10)

    elf1 = (elf1 + 1 + scoreboard[elf1]) % scoreboard.size
    elf2 = (elf2 + 1 + scoreboard[elf2]) % scoreboard.size
  end

  scoreboard[input, 10].join
end

input = File.read('input.txt').strip.to_i
puts solve_chocolate_charts(input)
