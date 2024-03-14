class Position
  attr_accessor :x, :y, :risk

  def initialize(x, y, risk)
    @x, @y, @risk = x, y, risk
  end
end

class PriorityQueue
  def initialize
    @heap = []
  end

  def push(position)
    @heap.push(position)
    heapify_up(@heap.length - 1)
  end

  def pop
    return nil if @heap.empty?

    root = @heap[0]
    @heap[0] = @heap.pop
    heapify_down(0)
    root
  end

  def empty?
    @heap.empty?
  end

  private

  def heapify_up(index)
    parent_index = (index - 1) / 2
    return if parent_index < 0 || @heap[parent_index].risk <= @heap[index].risk

    @heap[parent_index], @heap[index] = @heap[index], @heap[parent_index]
    heapify_up(parent_index)
  end

  def heapify_down(index)
    left_child_index = 2 * index + 1
    right_child_index = 2 * index + 2
    smallest_index = index

    if left_child_index < @heap.length && @heap[left_child_index].risk < @heap[smallest_index].risk
      smallest_index = left_child_index
    end

    if right_child_index < @heap.length && @heap[right_child_index].risk < @heap[smallest_index].risk
      smallest_index = right_child_index
    end

    if smallest_index != index
      @heap[index], @heap[smallest_index] = @heap[smallest_index], @heap[index]
      heapify_down(smallest_index)
    end
  end
end

def dijkstra(grid)
  pq = PriorityQueue.new
  pq.push(Position.new(0, 0, 0))

  rows, cols = grid.length, grid[0].length
  dist = Array.new(rows) { Array.new(cols, Float::INFINITY) }
  dist[0][0] = 0

  directions = [Position.new(1, 0, 0), Position.new(0, 1, 0), Position.new(-1, 0, 0), Position.new(0, -1, 0)]

  until pq.empty?
    curr = pq.pop
    return curr.risk if curr.x == rows - 1 && curr.y == cols - 1

    directions.each do |d|
      nx, ny = curr.x + d.x, curr.y + d.y
      next if nx < 0 || ny < 0 || nx >= rows || ny >= cols

      next_risk = curr.risk + grid[nx][ny]
      if next_risk < dist[nx][ny]
        dist[nx][ny] = next_risk
        pq.push(Position.new(nx, ny, next_risk))
      end
    end
  end

  -1
end

def extend_grid(initial_grid)
  rows, cols = initial_grid.length, initial_grid[0].length
  extended_grid = Array.new(rows * 5) { Array.new(cols * 5, 0) }

  (0...rows * 5).each do |i|
    (0...cols * 5).each do |j|
      new_risk = initial_grid[i % rows][j % cols] + i / rows + j / cols
      new_risk = (new_risk - 1) % 9 + 1
      extended_grid[i][j] = new_risk
    end
  end

  extended_grid
end

File.open("input.txt", "r") do |file|
  initial_grid = []
  file.each_line do |line|
    initial_grid << line.chomp.split("").map(&:to_i)
  end

  extended_grid = extend_grid(initial_grid)
  puts dijkstra(extended_grid)
end