
instructions = File.read("input.txt").strip
floor = 0
basement_position = nil

instructions.chars.each_with_index do |char, index|
  floor += char == '(' ? 1 : -1
  if floor == -1 && basement_position.nil?
    basement_position = index + 1
  end
end

puts floor
puts basement_position
