def parse_regex(regex)
  x, y = 0, 0
  stack = []
  positions = Set.new([[0, 0]])
  max_distance = 0
  distance = 0

  regex[1..-2].each_char do |char|
    case char
    when 'N', 'S', 'E', 'W'
      dx, dy = {'N' => [0, -1], 'S' => [0, 1], 'E' => [1, 0], 'W' => [-1, 0]}[char]
      x, y = x + dx, y + dy
      distance += 1
      positions.add([x, y])
      max_distance = [max_distance, distance].max
    when '('
      stack.push([x, y, distance])
    when '|'
      x, y, distance = stack.last
    when ')'
      stack.pop
    end
  end

  max_distance
end

input = File.read('input.txt').strip
result = parse_regex(input)
puts result
