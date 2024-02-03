
class Card
  attr_accessor :winnings, :givens, :totalCount

  def initialize(winnings, givens)
    @winnings = winnings
    @givens = givens
    @totalCount = 1
  end
end

def get_points_for_card(card)
  points = 0
  card.givens.each do |given, count|
    if card.winnings.key?(given)
      points += count * card.winnings[given]
    end
  end
  points
end

def lex_line_into_card(line)
  card_data_str = line.split(": ")[1]
  card_data = card_data_str.split(" | ")

  winnings = Hash.new(0)
  card_data[0].scan(/\d{1,2}/).each { |point| winnings[point] += 1 }

  givens = Hash.new(0)
  card_data[1].scan(/\d{1,2}/).each { |point| givens[point] += 1 }

  Card.new(winnings, givens)
end

file = File.read("input.txt")
input = file.strip

cards = []
input.split("\n").each do |line|
  next if line.empty?
  card = lex_line_into_card(line)
  cards << card
end

cards.each_with_index do |card, i|
  points = get_points_for_card(card)
  (1..points).each do |j|
    cards[i + j].totalCount += 1 * card.totalCount
  end
end

total_cards = cards.sum { |card| card.totalCount }
puts total_cards
