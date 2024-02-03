
coordinates = File.readlines('input.txt').map { |line| line.split(', ').map(&:to_i) }

max_x = coordinates.map { |coord| coord[0] }.max
max_y = coordinates.map { |coord| coord[1] }.max

grid = Array.new(max_x + 1) { Array.new(max_y + 1) }
areas = Hash.new(0)

(max_x + 1).times do |x|
  (max_y + 1).times do |y|
    distances = coordinates.map.with_index { |coord, index| [index, (x - coord[0]).abs + (y - coord[1]).abs] }
    min_distance = distances.min_by { |dist| dist[1] }

    if distances.count { |dist| dist[1] == min_distance[1] } == 1
      grid[x][y] = min_distance[0]
      areas[min_distance[0]] += 1
    else
      grid[x][y] = -1
    end
  end
end

infinite_areas = []

(max_x + 1).times do |x|
  infinite_areas << grid[x][0]
  infinite_areas << grid[x][max_y]
end

(max_y + 1).times do |y|
  infinite_areas << grid[0][y]
  infinite_areas << grid[max_x][y]
end

infinite_areas.uniq!
infinite_areas.delete(-1)

result = areas.reject { |key, value| infinite_areas.include?(key) }.values.max
puts result
