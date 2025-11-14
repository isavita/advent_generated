
matrix = File.read_lines("input.txt").map(&.chars)
h, w = matrix.size, matrix[0].size
visited = Array.new(h) { Array.new(w, false) }
sum = 0

h.times do |y|
  x = 0
  while x < w
    next (x += 1) if visited[y][x] || !matrix[y][x].ascii_number?
    x0 = x
    num = 0
    while x < w && matrix[y][x].ascii_number?
      num = num * 10 + matrix[y][x].to_u64
      visited[y][x] = true
      x += 1
    end
    len = x - x0
    adjacent = false
    (y-1..y+1).each do |yy|
      next if yy < 0 || yy >= h
      (x0-1..x).each do |xx|
        next if xx < 0 || xx >= w
        c = matrix[yy][xx]
        adjacent = true if c != '.' && !c.ascii_number?
      end
    end
    sum += num if adjacent
  end
end

puts sum
