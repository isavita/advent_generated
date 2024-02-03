
input = File.read('input.txt').to_i

def part_one(input)
  Math.sqrt(input).ceil / 2
end

def part_two(input)
  values = { [0, 0] => 1 }
  directions = [[1, 0], [0, 1], [-1, 0], [0, -1]]
  x, y = 0, 0
  direction = 0

  while true
    x += directions[direction][0]
    y += directions[direction][1]

    sum = 0
    (-1..1).each do |dx|
      (-1..1).each do |dy|
        sum += values[[x + dx, y + dy]] || 0
      end
    end

    values[[x, y]] = sum

    return sum if sum > input

    if [x + directions[(direction + 1) % 4][0], y + directions[(direction + 1) % 4][1]] != nil && values[[x + directions[(direction + 1) % 4][0], y + directions[(direction + 1) % 4][1]]].nil?
      direction = (direction + 1) % 4
    end
  end
end

puts part_one(input)
puts part_two(input)
