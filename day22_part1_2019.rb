
deck = (0..10006).to_a
File.open("input.txt").each do |line|
  if line.include?("deal into new stack")
    deck.reverse!
  elsif line.include?("cut")
    n = line.split(" ").last.to_i
    deck.rotate!(n)
  elsif line.include?("deal with increment")
    n = line.split(" ").last.to_i
    new_deck = deck.dup
    pos = 0
    deck.each do |card|
      new_deck[pos] = card
      pos = (pos + n) % deck.size
    end
    deck = new_deck
  end
end
puts deck.index(2019)
