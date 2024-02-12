
file = File.read("input.txt").chomp
floor = 0
position = 0

file.each_char.with_index do |c, i|
  if c == '('
    floor += 1
  elsif c == ')'
    floor -= 1
  end

  if floor == -1
    position = i + 1
    break
  end
end

puts position
