
def calculate_score(deck)
  deck.reverse.each_with_index.map { |card, index| card * (index + 1) }.sum
end

input = File.read('input.txt').split("\n\n")
player1 = input[0].split("\n")[1..-1].map(&:to_i)
player2 = input[1].split("\n")[1..-1].map(&:to_i)

while !player1.empty? && !player2.empty?
  card1 = player1.shift
  card2 = player2.shift

  if card1 > card2
    player1 += [card1, card2]
  else
    player2 += [card2, card1]
  end
end

winner = player1.empty? ? player2 : player1
puts calculate_score(winner)
