input = File.read('input.txt').split(", ")

direction = 0
x = 0
y = 0
visited = [[0, 0]]
found = false

input.each do |step|
  step.strip!
  turn = step[0]
  distance = step[1..-1].to_i

  if turn == "R"
    direction = (direction + 1) % 4
  else
    direction = (direction - 1) % 4
  end

  distance.times do
    case direction
    when 0
      y += 1
    when 1
      x += 1
    when 2
      y -= 1
    when 3
      x -= 1
    end

    if !found && visited.include?([x, y])
      puts x.abs + y.abs
      found = true
    end

    visited << [x, y]
  end
end