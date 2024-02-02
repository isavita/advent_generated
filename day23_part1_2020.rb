
input = File.read('input.txt').chomp.chars.map(&:to_i)

def play_game(cups, moves)
  current_index = 0
  max_cup = cups.max
  min_cup = cups.min

  moves.times do
    current_cup = cups[current_index]
    pick_up = []
    3.times do
      pick_up << cups.delete_at((cups.index(current_cup) + 1) % cups.length)
    end

    destination_cup = current_cup - 1
    while pick_up.include?(destination_cup) || destination_cup < min_cup
      destination_cup -= 1
      destination_cup = max_cup if destination_cup < min_cup
    end

    destination_index = cups.index(destination_cup)
    cups.insert(destination_index + 1, *pick_up)

    current_index = (cups.index(current_cup) + 1) % cups.length
  end

  cups.rotate(cups.index(1))[1..-1].join
end

puts play_game(input, 100)
