
def deal_into_new_stack(deck : Array(Int32)) : Array(Int32)
  (0...deck.size/2).each do |i|
    deck[i], deck[deck.size-i-1] = deck[deck.size-i-1], deck[i]
  end
  deck
end

def cut_n(deck : Array(Int32), n : Int32) : Array(Int32)
  if n >= 0
    deck[n..-1] + deck[0...n]
  else
    deck[deck.size+n..-1] + deck[0...deck.size+n]
  end
end

def deal_with_increment(deck : Array(Int32), n : Int32) : Array(Int32)
  new_deck = Array(Int32).new(deck.size, 0)

  deck.each_with_index do |card, i|
    new_deck[(i*n) % deck.size] = card
  end

  new_deck
end

def find_2019(deck : Array(Int32)) : Int32
  deck.index(2019) || -1
end

size = 10007
deck = (0...size).to_a

File.open("input.txt") do |file|
  file.each_line do |line|
    line = line.chomp

    case line
    when "deal into new stack"
      deck = deal_into_new_stack(deck)
    when /^cut/
      n = line.split(" ")[1].to_i
      deck = cut_n(deck, n)
    when /^deal with increment/
      n = line.split(" ").last.to_i
      deck = deal_with_increment(deck, n)
    end
  end
end

puts find_2019(deck)
