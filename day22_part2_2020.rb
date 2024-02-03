
def play_game(p1, p2, recursive = false)
  seen = {}
  while !p1.empty? && !p2.empty?
    if recursive
      return [1, p1] if seen[[p1, p2]]
      seen[[p1.dup, p2.dup]] = true
    end

    c1, c2 = p1.shift, p2.shift
    if recursive && p1.size >= c1 && p2.size >= c2
      winner, = play_game(p1.take(c1), p2.take(c2), true)
    else
      winner = c1 > c2 ? 1 : 2
    end

    if winner == 1
      p1.push(c1, c2)
    else
      p2.push(c2, c1)
    end
  end
  p1.empty? ? [2, p2] : [1, p1]
end

def calculate_score(deck)
  deck.reverse.each_with_index.sum { |card, index| card * (index + 1) }
end

input = File.read("input.txt").split("\n\n")
p1 = input[0].lines[1..].map(&:to_i)
p2 = input[1].lines[1..].map(&:to_i)

_, winning_deck = play_game(p1.dup, p2.dup)
puts calculate_score(winning_deck)

_, winning_deck_recursive = play_game(p1, p2, true)
puts calculate_score(winning_deck_recursive)
