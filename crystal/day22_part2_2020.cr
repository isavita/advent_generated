require "file_utils"

class Deck < Array(Int32)
  def copy(n)
    copy = Deck.new
    n.times { |i| copy.push(self[i]) }
    copy
  end

  def score
    score = 0
    each_with_index { |card, i| score += card * (size - i) }
    score
  end
end

def play_recursive_combat(player1, player2)
  previous_rounds = {} of String => Bool
  while player1.size > 0 && player2.size > 0
    round_key = "#{player1}|#{player2}"
    if previous_rounds[round_key]?
      return player1, Deck.new
    end
    previous_rounds[round_key] = true

    card1, card2 = player1.shift, player2.shift

    if player1.size >= card1 && player2.size >= card2
      sub_player1, _ = play_recursive_combat(player1.copy(card1), player2.copy(card2))
      if sub_player1.size > 0
        player1.push(card1, card2)
      else
        player2.push(card2, card1)
      end
    else
      if card1 > card2
        player1.push(card1, card2)
      else
        player2.push(card2, card1)
      end
    end
  end
  return player1, player2
end

file = File.open("input.txt")
player1_deck, player2_deck = Deck.new, Deck.new
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
  current_deck.push(card)
end

player1_deck, player2_deck = play_recursive_combat(player1_deck, player2_deck)

winning_deck = player1_deck.size > 0 ? player1_deck : player2_deck
puts winning_deck.score