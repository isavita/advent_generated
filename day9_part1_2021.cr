
file = File.open("input.txt")
heightmap = file.gets_to_end.split("\n").map { |line| line.chars.map { |char| char.to_i } }

total_risk_level = 0
heightmap.each_with_index do |row, y|
  row.each_with_index do |height, x|
    if is_low_point(heightmap, x, y)
      total_risk_level += 1 + height
    end
  end
end

puts total_risk_level

def is_low_point(heightmap, x, y)
  height = heightmap[y][x]
  return false if x > 0 && heightmap[y][x-1] <= height
  return false if x < heightmap[y].size - 1 && heightmap[y][x+1] <= height
  return false if y > 0 && heightmap[y-1][x] <= height
  return false if y < heightmap.size - 1 && heightmap[y+1][x] <= height
  true
end
