file = File.open("input.txt")
player1_deck, player2_deck = [] of Int32, [] of Int32
current_deck = player1_deck
file.each_line do |line|
  if line.strip.empty?
    current_deck = player2_deck
    next
  end
  if line.includes?("Player")
    next
  end
  card = line.to_i
  current_deck << card
end
file.close

while player1_deck.any? && player2_deck.any?
  card1, card2 = player1_deck.shift, player2_deck.shift
  if card1 > card2
    player1_deck += [card1, card2]
  else
    player2_deck += [card2, card1]
  end
end

winning_deck = player1_deck.any? ? player1_deck : player2_deck
score = winning_deck.each_with_index.sum { |card, i| card * (winning_deck.size - i) }
puts score