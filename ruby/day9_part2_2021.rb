
heightmap = File.readlines('input.txt').map { |line| line.chomp.chars.map(&:to_i) }

basin_sizes = []
visited = {}

def is_low_point(heightmap, x, y)
  height = heightmap[y][x]
  return false if x > 0 && heightmap[y][x - 1] <= height
  return false if x < heightmap[y].length - 1 && heightmap[y][x + 1] <= height
  return false if y > 0 && heightmap[y - 1][x] <= height
  return false if y < heightmap.length - 1 && heightmap[y + 1][x] <= height
  true
end

def explore_basin(heightmap, x, y, visited)
  return 0 if visited[[x, y]] || heightmap[y][x] == 9
  visited[[x, y]] = true
  size = 1

  directions = [[0, -1], [-1, 0], [0, 1], [1, 0]]
  directions.each do |dir|
    new_x, new_y = x + dir[0], y + dir[1]
    if new_x >= 0 && new_x < heightmap[0].length && new_y >= 0 && new_y < heightmap.length
      size += explore_basin(heightmap, new_x, new_y, visited)
    end
  end
  size
end

heightmap.each_with_index do |row, y|
  row.each_index do |x|
    if is_low_point(heightmap, x, y)
      size = explore_basin(heightmap, x, y, visited)
      basin_sizes << size
    end
  end
end

basin_sizes.sort!.reverse!
result = basin_sizes[0] * basin_sizes[1] * basin_sizes[2]
puts result
