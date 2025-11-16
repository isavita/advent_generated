
alias Point = Tuple(Int32, Int32)

def dijkstra(grid : Array(String), end_p : Point) : Hash(Point, Int32)
  dist = Hash(Point, Int32).new
  pq = PriorityQueue(Tuple(Int32, Point)).new
  pq.push({0, end_p})
  dist[end_p] = 0
  rows = grid.size
  cols = grid[0].size
  dr = {-1, 1, 0, 0}
  dc = {0, 0, -1, 1}
  until pq.empty?
    curr_dist, curr = pq.pop
    next if curr_dist > dist[curr]
    4.times do |i|
      next_p = {curr[0] + dr[i], curr[1] + dc[i]}
      next if next_p[0] < 0 || next_p[0] >= rows || next_p[1] < 0 || next_p[1] >= cols
      next if grid[curr[0]][curr[1]].ord - grid[next_p[0]][next_p[1]].ord > 1
      next_dist = curr_dist + 1
      if !dist.has_key?(next_p) || next_dist < dist[next_p]
        dist[next_p] = next_dist
        pq.push({next_dist, next_p})
      end
    end
  end
  dist
end

class PriorityQueue(T)
  def initialize
    @heap = Array(T).new
  end
  def push(v : T)
    @heap << v
    heapify_up(@heap.size - 1)
  end
  def pop : T
    raise "empty" if empty?
    swap(0, @heap.size - 1)
    val = @heap.pop
    heapify_down(0) unless empty?
    val
  end
  def empty? : Bool
    @heap.empty?
  end
  private def heapify_up(i)
    return if i == 0
    parent = (i - 1) // 2
    if @heap[i][0] < @heap[parent][0]
      swap(i, parent)
      heapify_up(parent)
    end
  end
  private def heapify_down(i)
    l = 2*i + 1
    r = 2*i + 2
    smallest = i
    smallest = l if l < @heap.size && @heap[l][0] < @heap[smallest][0]
    smallest = r if r < @heap.size && @heap[r][0] < @heap[smallest][0]
    if smallest != i
      swap(i, smallest)
      heapify_down(smallest)
    end
  end
  private def swap(i, j)
    @heap[i], @heap[j] = @heap[j], @heap[i]
  end
end

grid_data = [] of String
start_pos = {0, 0}
end_pos = {0, 0}
a_positions = [] of Point
File.read_lines("input.txt").each_with_index do |line, y|
  grid_data << line
  line.chars.each_with_index do |c, x|
    p = {y, x}
    case c
    when 'S' then start_pos = p
    when 'E' then end_pos = p
    when 'a' then a_positions << p
    end
  end
end
grid_data[start_pos[0]] = grid_data[start_pos[0]].tr("S", "a")
grid_data[end_pos[0]] = grid_data[end_pos[0]].tr("E", "z")
dists = dijkstra(grid_data, end_pos)
shortest = dists.fetch(start_pos, Int32::MAX)
a_positions.each { |p| shortest = {shortest, dists.fetch(p, Int32::MAX)}.min }
puts shortest
