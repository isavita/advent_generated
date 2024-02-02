
input = File.read('input.txt').split(', ')

direction = 0
x = 0
y = 0

input.each do |step|
  turn = step[0]
  distance = step[1..-1].to_i

  if turn == 'R'
    direction = (direction + 1) % 4
  else
    direction = (direction - 1) % 4
  end

  case direction
  when 0
    y += distance
  when 1
    x += distance
  when 2
    y -= distance
  when 3
    x -= distance
  end
end

puts x.abs + y.abs
