
class Coord
  getter x : Int32
  getter y : Int32

  def initialize(@x : Int32, @y : Int32) end
  def add(other : Coord) : Coord; Coord.new(x + other.x, y + other.y) end
  def sub(other : Coord) : Coord; Coord.new(x - other.x, y - other.y) end
  def ==(other : Coord) : Bool; x == other.x && y == other.y end
  def opposite : Coord; Coord.new(-x, -y) end
end

class State
  property coord : Coord
  property dir : Coord
  property num_straight : Int32

  def initialize(@coord : Coord, @dir : Coord, @num_straight : Int32) end
end

record HeapNode, priority : Int32, cost : Int32, state : State

class PriorityQueue
  getter nodes : Array(HeapNode)
  getter size : Int32

  def initialize
    @nodes = [] of HeapNode
    @size = 0
  end

  def empty? : Bool; @size == 0 end

  def << (node : HeapNode)
    @nodes << node
    @size += 1
    heapify_up(@size - 1)
  end

  def pop : HeapNode
    root = @nodes[0]
    @nodes[0] = @nodes[@size - 1]
    @size -= 1
    @nodes.pop
    heapify_down(0) if @size > 0
    root
  end

  private def heapify_up(index : Int32)
    while index > 0
      parent = (index - 1) // 2
      break if @nodes[index].priority >= @nodes[parent].priority
      @nodes.swap(index, parent)
      index = parent
    end
  end

  private def heapify_down(index : Int32)
    loop do
      left = 2 * index + 1
      right = 2 * index + 2
      smallest = index
      smallest = left if left < @size && @nodes[left].priority < @nodes[smallest].priority
      smallest = right if right < @size && @nodes[right].priority < @nodes[smallest].priority
      break if smallest == index
      @nodes.swap(index, smallest)
      index = smallest
    end
  end
end

DIRS = {Coord.new(0, -1), Coord.new(-1, 0), Coord.new(0, 1), Coord.new(1, 0)}
DIR_IDX = DIRS.to_h { |d| {d, DIRS.index(d).not_nil!} }
START_DIR = Coord.new(0, 0)

def heuristic(a : Coord, b : Coord) : Int32
  (a.x - b.x).abs + (a.y - b.y).abs
end

def a_star(grid : Array(Array(Int32)), start : Coord, goal : Coord, min_straight : Int32, max_straight : Int32) : Int32
  h = grid.size
  w = grid[0].size
  cost_so_far = Array.new(h) { Array.new(w) { Array.new(5) { Array.new(max_straight + 1, Int32::MAX) } } }
  frontier = PriorityQueue.new
  start_state = State.new(start, START_DIR, 0)
  cost_so_far[start.y][start.x][4][0] = 0
  frontier << HeapNode.new(heuristic(start, goal), 0, start_state)
  best = Int32::MAX

  until frontier.empty?
    curr = frontier.pop
    state = curr.state
    c = state.coord
    d = state.dir
    n = state.num_straight
    d_idx = d == START_DIR ? 4 : DIR_IDX[d]
    next if curr.cost > cost_so_far[c.y][c.x][d_idx][n]
    best = curr.cost if c == goal && curr.cost < best
    next if curr.cost >= best
    DIRS.each do |new_dir|
      nc = c.add(new_dir)
      next unless (0...w).covers?(nc.x) && (0...h).covers?(nc.y)
      next if d != START_DIR && new_dir == d.opposite
      new_n = new_dir == d ? n + 1 : 1
      next if new_n > max_straight
      if d != START_DIR && new_dir != d && n < min_straight
        next
      end
      new_cost = curr.cost + grid[nc.y][nc.x]
      new_d_idx = DIR_IDX[new_dir]
      if new_cost < cost_so_far[nc.y][nc.x][new_d_idx][new_n]
        cost_so_far[nc.y][nc.x][new_d_idx][new_n] = new_cost
        priority = new_cost + heuristic(nc, goal)
        frontier << HeapNode.new(priority, new_cost, State.new(nc, new_dir, new_n))
      end
    end
  end
  best == Int32::MAX ? -1 : best
end

grid = File.read_lines("input.txt").map { |l| l.chars.map { |c| c.to_i } }
start = Coord.new(0, 0)
goal = Coord.new(grid[0].size - 1, grid.size - 1)
puts a_star(grid, start, goal, 0, 3)
