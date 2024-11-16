
def hand_type(hand : String) : Int32
  counts = hand.chars.group_by(&.itself).transform_values(&.size)
  
  case counts.size
  when 1 # Five of a kind
    return 7
  when 2
    return counts.values.includes?(4) ? 6 : 5 # Four of a kind or Full house
  when 3
    return counts.values.includes?(3) ? 4 : 3 # Three of a kind or Two pair
  when 4
    return 2 # One pair
  else
    return 1 # High card
  end
end

def card_strength(card : Char) : Int32
  strengths = {
    'A' => 14, 'K' => 13, 'Q' => 12, 'J' => 11, 
    'T' => 10, '9' => 9, '8' => 8, '7' => 7, 
    '6' => 6, '5' => 5, '4' => 4, '3' => 3, '2' => 2
  }
  strengths[card]
end

def compare_hands(hand1 : String, hand2 : String) : Int32
  type1 = hand_type(hand1)
  type2 = hand_type(hand2)
  
  return type1 <=> type2 if type1 != type2
  
  # If types are equal, compare card by card
  hand1.chars.zip(hand2.chars).each do |c1, c2|
    strength_diff = card_strength(c1) <=> card_strength(c2)
    return strength_diff if strength_diff != 0
  end
  
  0
end

# Read input
hands_and_bids = File.read_lines("input.txt").map do |line|
  hand, bid = line.split
  {hand, bid.to_i}
end

# Sort hands
sorted_hands = hands_and_bids.sort do |a, b|
  compare_hands(a[0], b[0])
end

# Calculate total winnings
total_winnings = sorted_hands.map_with_index do |hand_bid, index|
  hand_bid[1] * (index + 1)
end.sum

puts total_winnings
