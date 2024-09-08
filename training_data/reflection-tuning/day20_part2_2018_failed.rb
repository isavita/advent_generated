def explore(regex)
  x, y = 0, 0
  positions = [[0, 0]]
  distances = {[0, 0] => 0}
  max_distance = 0
  rooms_over_1000 = 0

  stack = []

  regex.each_char do |char|
    case char
    when 'N', 'S', 'E', 'W'
      dx, dy = {'N' => [0, -1], 'S' => [0, 1], 'E' => [1, 0], 'W' => [-1, 0]}[char]
      x, y = x + dx, y + dy
      current_distance = distances[[x, y]] || (distances[[x-dx, y-dy]] + 1)
      distances[[x, y]] = [distances[[x, y]] || Float::INFINITY, current_distance].min
      max_distance = [max_distance, current_distance].max
      rooms_over_1000 += 1 if current_distance >= 1000 && !distances[[x, y]]
      positions << [x, y]
    when '('
      stack.push(positions.dup)
    when '|'
      positions = stack.last.dup
      x, y = positions.last
    when ')'
      stack.pop
    end
  end

  [max_distance, rooms_over_1000]
end

input = File.read('input.txt').strip
max_doors, rooms_over_1000 = explore(input[1..-2])  # Remove ^ and $

puts "Part 1: #{max_doors}"
puts "Part 2: #{rooms_over_1000}"
