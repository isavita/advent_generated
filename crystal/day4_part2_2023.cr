class Card
  property winnings : Hash(String, Int32)
  property givens : Hash(String, Int32)
  property total_count : Int32

  def initialize
    @winnings = {} of String => Int32
    @givens = {} of String => Int32
    @total_count = 1
  end
end

def get_points_for_card(card : Card) : Int32
  points = 0
  card.givens.each do |given, count|
    if card.winnings.has_key?(given)
      points += count * card.winnings[given]
    end
  end
  points
end

def lex_line_into_card(line : String) : Card
  _, card_data_str = line.split(": ", 2)
  card_data = card_data_str.split(" | ")

  winnings = {} of String => Int32
  card_data[0].scan(/\d{1,2}/) do |point|
    winnings[point.to_s] ||= 0
    winnings[point.to_s] += 1
  end

  givens = {} of String => Int32
  card_data[1].scan(/\d{1,2}/) do |point|
    givens[point.to_s] ||= 0
    givens[point.to_s] += 1
  end

  card = Card.new
  card.winnings = winnings
  card.givens = givens
  card
end

file = File.read("input.txt").strip
cards = [] of Card

file.lines.each do |line|
  next if line.empty?
  cards << lex_line_into_card(line)
end

cards.each_with_index do |card, i|
  points = get_points_for_card(card)
  (1..points).each do |j|
    next if i + j >= cards.size # Prevent out-of-bounds access
    cards[i + j].total_count += card.total_count
  end
end

total_cards = cards.reduce(0) { |sum, card| sum + card.total_count }
puts total_cards