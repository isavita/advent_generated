
def read_heightmap
  File.read('input.txt').lines.map { |line| line.chomp.chars.map(&:to_i) }
end

def low_points(heightmap)
  low_points = []
  heightmap.each_with_index do |row, y|
    row.each_with_index do |height, x|
      adjacent = []
      adjacent << heightmap[y-1][x] if y > 0
      adjacent << heightmap[y+1][x] if y < heightmap.size-1
      adjacent << heightmap[y][x-1] if x > 0
      adjacent << heightmap[y][x+1] if x < row.size-1
      low_points << height if adjacent.all? { |adj_height| height < adj_height }
    end
  end
  low_points
end

def risk_level_sum(low_points)
  low_points.sum { |point| point + 1 }
end

heightmap = read_heightmap
low_points = low_points(heightmap)
puts risk_level_sum(low_points)
