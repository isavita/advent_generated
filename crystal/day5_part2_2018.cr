
def react(polymer : String) : String
  stack = [] of Char
  polymer.each_char do |unit|
    if stack.empty?
      stack.push(unit)
    elsif stack.last != unit && stack.last.upcase == unit.upcase
      stack.pop
    else
      stack.push(unit)
    end
  end
  stack.join
end

content = File.read("input.txt").chomp

min_length = content.size
('a'..'z').each do |unit|
  temp_polymer = content.gsub(unit.to_s, "").gsub(unit.to_s.upcase, "")
  reacted_polymer = react(temp_polymer)
  min_length = reacted_polymer.size if reacted_polymer.size < min_length
end

puts min_length
