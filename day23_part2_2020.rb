
input = File.read("input.txt").chomp.chars.map(&:to_i)

def play_game(cups, moves)
  current = cups.first
  max = cups.max
  min = cups.min

  cups_hash = {}
  cups.each_with_index { |cup, i| cups_hash[cup] = cups[(i + 1) % cups.length] }

  moves.times do
    pick_up = [cups_hash[current], cups_hash[cups_hash[current]], cups_hash[cups_hash[cups_hash[current]]]]
    destination = current - 1 < min ? max : current - 1
    while pick_up.include?(destination)
      destination = destination - 1 < min ? max : destination - 1
    end

    cups_hash[current] = cups_hash[pick_up.last]
    cups_hash[pick_up.last] = cups_hash[destination]
    cups_hash[destination] = pick_up.first

    current = cups_hash[current]
  end

  cups_hash
end

# Part 1
cups_part_1 = play_game(input, 100)
result_part_1 = ""
cup = cups_part_1[1]
8.times do
  result_part_1 += cup.to_s
  cup = cups_part_1[cup]
end
puts result_part_1

# Part 2
input += (input.max + 1..1_000_000).to_a
cups_part_2 = play_game(input, 10_000_000)
cup_1 = cups_part_2[1]
cup_2 = cups_part_2[cup_1]
puts cup_1 * cup_2
