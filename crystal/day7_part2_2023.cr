
module CamelCards
  CARD_VALUES = "AKQT98765432J"

  enum HandType
    FiveOfAKind
    FourOfAKind
    FullHouse
    ThreeOfAKind
    TwoPair
    OnePair
    HighCard
  end

  def self.hand_type(hand : String) : HandType
    counts = Hash(Char, Int32).new(0)
    jokers = 0
    hand.each_char do |card|
      if card == 'J'
        jokers += 1
      else
        counts[card] += 1
      end
    end

    sorted_counts = counts.values.sort.reverse
    if jokers == 5 || sorted_counts[0] + jokers == 5
      HandType::FiveOfAKind
    elsif sorted_counts[0] + jokers == 4
      HandType::FourOfAKind
    elsif sorted_counts[0] + jokers == 3 && sorted_counts[1] == 2
      HandType::FullHouse
    elsif sorted_counts[0] + jokers == 3
      HandType::ThreeOfAKind
    elsif sorted_counts[0] == 2 && sorted_counts[1] == 2
      HandType::TwoPair
    elsif sorted_counts[0] + jokers == 2
      HandType::OnePair
    else
      HandType::HighCard
    end
  end

  def self.compare_hands(hand1 : String, hand2 : String) : Int32
    type1 = hand_type(hand1)
    type2 = hand_type(hand2)

    if type1 != type2
      return type2 <=> type1
    end

    hand1.each_char.with_index do |card1, i|
      card2 = hand2[i]
      if card1 != card2
        return CARD_VALUES.index(card2).not_nil!.<=> CARD_VALUES.index(card1).not_nil!
      end
    end

    0
  end

  def self.calculate_winnings(hands_and_bids : Array(Tuple(String, Int32))) : Int64
    sorted_hands = hands_and_bids.sort do |a, b|
      compare_hands(a[0], b[0])
    end

    total_winnings = 0_i64
    sorted_hands.each_with_index do |(hand, bid), rank|
      total_winnings += bid * (rank + 1)
    end

    total_winnings
  end
end

lines = File.read_lines("input.txt")
hands_and_bids = lines.map do |line|
  hand, bid_str = line.split
  {hand, bid_str.to_i}
end

puts CamelCards.calculate_winnings(hands_and_bids)
