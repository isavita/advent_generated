
data = File.read("input.txt").chomp

visited_houses = {} of Tuple(Int32, Int32) => Bool
x = 0
y = 0

visited_houses[{x, y}] = true

data.each_char do |dir|
  case dir
  when '^'
    y += 1
  when 'v'
    y -= 1
  when '>'
    x += 1
  when '<'
    x -= 1
  end

  visited_houses[{x, y}] = true
end

puts visited_houses.size
