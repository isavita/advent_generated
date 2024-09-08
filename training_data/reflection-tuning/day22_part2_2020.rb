def parse_input(filename)
  players = File.read(filename).split("\n\n").map { |p| p.split("\n")[1..-1].map(&:to_i) }
  [players[0], players[1]]
end

def play_combat(deck1, deck2)
  until deck1.empty? || deck2.empty?
    card1, card2 = deck1.shift, deck2.shift
    if card1 > card2
      deck1.push(card1, card2)
    else
      deck2.push(card2, card1)
    end
  end
  deck1.empty? ? deck2 : deck1
end

def play_recursive_combat(deck1, deck2)
  seen = Set.new
  until deck1.empty? || deck2.empty?
    config = [deck1.dup, deck2.dup]
    return [1, deck1] if seen.include?(config)
    seen.add(config)

    card1, card2 = deck1.shift, deck2.shift
    if deck1.size >= card1 && deck2.size >= card2
      winner, _ = play_recursive_combat(deck1.first(card1), deck2.first(card2))
    else
      winner = card1 > card2 ? 1 : 2
    end

    if winner == 1
      deck1.push(card1, card2)
    else
      deck2.push(card2, card1)
    end
  end
  deck1.empty? ? [2, deck2] : [1, deck1]
end

def calculate_score(deck)
  deck.reverse.map.with_index(1) { |card, i| card * i }.sum
end

deck1, deck2 = parse_input("input.txt")

# Part 1
winning_deck = play_combat(deck1.dup, deck2.dup)
puts "Part 1: #{calculate_score(winning_deck)}"

# Part 2
_, winning_deck = play_recursive_combat(deck1, deck2)
puts "Part 2: #{calculate_score(winning_deck)}"
