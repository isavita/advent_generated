
polymer = File.read('input.txt').chomp

def react(polymer)
  reacted = []

  polymer.each_char do |unit|
    if reacted.empty? || (reacted.last.ord - unit.ord).abs != 32
      reacted << unit
    else
      reacted.pop
    end
  end

  reacted.join
end

puts react(polymer).length

puts ('a'..'z').map { |unit| react(polymer.gsub(/#{unit}/i, '')).length }.min
