HIGH_CARD = 1
ONE_PAIR = 2
TWO_PAIR = 3
THREE_KIND = 4
FULL_HOUSE = 5
FOUR_KIND = 6
FIVE_KIND = 7

class Hand
  attr_reader :cards, :bid

  def initialize(cards, bid)
    @cards = cards
    @bid = bid
  end
end

class RankedHand
  attr_reader :hand, :rank

  def initialize(hand, rank)
    @hand = hand
    @rank = rank
  end
end

def find_matches(hands)
  matches = Array.new(7) { [] }

  hands.each do |hand|
    count = Hash.new(0)
    hand.cards.each_char { |card| count[card] += 1 }

    value = count.values.reduce(1) { |product, c| product * c }

    case value
    when 1
      matches[FIVE_KIND - 1] << hand
    when 2
      matches[FOUR_KIND - 1] << hand
    when 3
      matches[THREE_KIND - 1] << hand
    when 4
      if count.size == 2
        matches[ONE_PAIR - 1] << hand
      else
        matches[FULL_HOUSE - 1] << hand
      end
    when 5
      matches[HIGH_CARD - 1] << hand
    when 6
      matches[TWO_PAIR - 1] << hand
    else
      puts "oh no"
    end
  end

  matches
end

def convert_and_order_matches(matches)
  converted_matches = []

  matches.each do |category|
    temp = category.map do |hand|
      cards = hand.cards.tr('TJQKA', 'ABCDE')
      rank = cards.to_i(16)
      RankedHand.new(hand, rank)
    end

    temp.sort_by!(&:rank).reverse!
    converted_matches.concat(temp)
  end

  converted_matches
end

File.open("input.txt", "r") do |file|
  lines = file.readlines.map(&:chomp)

  hands = lines.map do |line|
    cards = line.match(/[\dAKQJT]+/)[0]
    bid = line.match(/ [\d]+/)[0][1..-1].to_i
    Hand.new(cards, bid)
  end

  matches = find_matches(hands)
  converted_matches = convert_and_order_matches(matches)

  total = 0
  converted_matches.each_with_index { |ranked_hand, i| total += ranked_hand.hand.bid * (converted_matches.length - i) }

  puts total
end