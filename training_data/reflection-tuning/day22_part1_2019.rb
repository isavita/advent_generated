def shuffle(position, deck_size, instructions)
  instructions.each do |instruction|
    case instruction
    when /deal into new stack/
      position = deck_size - 1 - position
    when /cut (-?\d+)/
      n = $1.to_i
      position = (position - n) % deck_size
    when /deal with increment (\d+)/
      n = $1.to_i
      position = (position * n) % deck_size
    end
  end
  position
end

deck_size = 10007
card_position = 2019
instructions = File.readlines('input.txt', chomp: true)

final_position = shuffle(card_position, deck_size, instructions)
puts final_position
