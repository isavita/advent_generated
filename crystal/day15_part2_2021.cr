
class PriorityQueue(T)
  def initialize
    @data = [] of T
  end

  def enqueue(element)
    @data << element
    @data.sort_by! { |e| e[2] }
  end

  def dequeue
    @data.shift
  end

  def empty?
    @data.empty?
  end
end

grid = File.read("input.txt").lines.map { |l| l.chomp.chars.map(&.to_i) }

rows = grid.size
cols = grid[0].size
ext_rows = rows * 5
ext_cols = cols * 5

ext = Array.new(ext_rows) { Array.new(ext_cols, 0) }
ext_rows.times do |i|
  ext_cols.times do |j|
    v = grid[i % rows][j % cols] + (i // rows) + (j // cols)
    ext[i][j] = v > 9 ? v - 9 : v
  end
end

dist = Array.new(ext_rows) { Array.new(ext_cols, Int32::MAX) }
dist[0][0] = 0
pq = PriorityQueue(Tuple(Int32, Int32, Int32)).new
pq.enqueue({0, 0, 0})

until pq.empty?
  x, y, r = pq.dequeue
  next if r > dist[x][y]
  break if x == ext_rows - 1 && y == ext_cols - 1

  [{1, 0}, {0, 1}, {-1, 0}, {0, -1}].each do |dx, dy|
    nx = x + dx
    ny = y + dy
    next unless 0 <= nx < ext_rows && 0 <= ny < ext_cols
    nr = r + ext[nx][ny]
    if nr < dist[nx][ny]
      dist[nx][ny] = nr
      pq.enqueue({nx, ny, nr})
    end
  end
end

puts dist[-1][-1]
