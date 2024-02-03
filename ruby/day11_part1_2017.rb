
input = File.read('input.txt').split(',')

directions = {
  'n' => [0, 1, -1],
  'ne' => [1, 0, -1],
  'se' => [1, -1, 0],
  's' => [0, -1, 1],
  'sw' => [-1, 0, 1],
  'nw' => [-1, 1, 0]
}

pos = [0, 0, 0]
max_distance = 0

input.each do |step|
  pos = pos.zip(directions[step]).map { |x, y| x + y }
  max_distance = [max_distance, pos.map(&:abs).max].max
end

puts pos.map(&:abs).max
puts max_distance
