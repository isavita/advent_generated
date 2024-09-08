# Read and parse input
player1, player2 = File.read('input.txt').split("\n\n").map { |p| p.split("\n")[1..-1].map(&:to_i) }

# Play the game
until player1.empty? || player2.empty?
  card1, card2 = player1.shift, player2.shift
  if card1 > card2
    player1.push(card1, card2)
  else
    player2.push(card2, card1)
  end
end

# Calculate score
winner = player1.empty? ? player2 : player1
score = winner.reverse.each_with_index.sum { |card, index| card * (index + 1) }

puts score
