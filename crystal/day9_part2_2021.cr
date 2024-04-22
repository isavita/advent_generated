# Read input from file
heightmap = File.read("input.txt").split("\n").map { |line| line.chars.map(&.to_i) }

# Find low points
low_points = [] of Tuple(Int32, Int32)
heightmap.each_with_index do |row, y|
  row.each_with_index do |height, x|
    is_low_point = true
    [-1, 1].each do |dy|
      if y + dy >= 0 && y + dy < heightmap.size
        is_low_point &&= height < heightmap[y + dy][x]
      end
    end
    [-1, 1].each do |dx|
      if x + dx >= 0 && x + dx < row.size
        is_low_point &&= height < row[x + dx]
      end
    end
    low_points << {x, y} if is_low_point
  end
end

# Calculate risk levels
risk_levels = low_points.map do |x, y|
  heightmap[y][x] + 1
end.sum

puts "Part 1: #{risk_levels}"

# Part 2
basin_sizes = [] of Int32
low_points.each do |x, y|
  basin_size = 0
  queue = [{x, y}]
  visited = Set(Tuple(Int32, Int32)).new
  while queue.any?
    cx, cy = queue.shift
    next if visited.includes?({cx, cy})
    visited.add({cx, cy})
    basin_size += 1
    [-1, 1].each do |dx|
      if cx + dx >= 0 && cx + dx < heightmap[cy].size
        if heightmap[cy][cx + dx] < 9
          queue << {cx + dx, cy}
        end
      end
    end
    [-1, 1].each do |dy|
      if cy + dy >= 0 && cy + dy < heightmap.size
        if heightmap[cy + dy][cx] < 9
          queue << {cx, cy + dy}
        end
      end
    end
  end
  basin_sizes << basin_size
end
basin_sizes.sort!.reverse!
puts "Part 2: #{basin_sizes[0] * basin_sizes[1] * basin_sizes[2]}"