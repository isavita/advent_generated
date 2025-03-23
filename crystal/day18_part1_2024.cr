
def solve
  grid_size = 71
  corrupted = Set(Tuple(Int32, Int32)).new
  
  File.open("input.txt") do |f|
    f.each_line.with_index do |line, i|
      break if i >= 1024
      x, y = line.strip.split(",").map(&.to_i32)
      corrupted.add({x, y})
    end
  end

  q = Deque(Tuple(Int32, Int32, Int32)).new
  q.push({0, 0, 0})
  visited = Set(Tuple(Int32, Int32)).new
  visited.add({0, 0})

  while !q.empty?
    x, y, steps = q.shift

    if x == grid_size - 1 && y == grid_size - 1
      puts steps
      return
    end

    [ {0, 1}, {0, -1}, {1, 0}, {-1, 0} ].each do |dx, dy|
      nx, ny = x + dx, y + dy
      if 0 <= nx < grid_size && 0 <= ny < grid_size && !corrupted.includes?({nx, ny}) && !visited.includes?({nx, ny})
        q.push({nx, ny, steps + 1})
        visited.add({nx, ny})
      end
    end
  end
end

solve
