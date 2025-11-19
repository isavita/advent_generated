
struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x, @y)
  end

  def to_s(io)
    io << x << ',' << y
  end
end

struct Step
  property p : Array(Point)
  property worker_id : Int32
  property keys : Set(Char)

  def initialize(@p, @worker_id, keys : Set(Char))
    @keys = keys.dup
  end

  def clone
    Step.new(p.dup, worker_id, keys)
  end

  def to_s(io)
    sorted = keys.to_a.sort.join
    p.each_with_index do |pt, i|
      io << pt
      io << '_' unless i == p.size - 1
    end
    io << '_' << worker_id << '_' << sorted
  end
end

class PriorityQueue
  struct Item
    property obj : Step
    property priority : Int32

    def initialize(@obj, @priority)
    end
  end

  def initialize
    @heap = [] of Item
  end

  def push(item : Item)
    @heap << item
    heapify_up
  end

  def pop?
    return nil if @heap.empty?
    @heap.swap(0, @heap.size - 1)
    item = @heap.pop
    heapify_down
    item
  end

  private def heapify_up
    idx = @heap.size - 1
    while idx > 0
      parent = (idx - 1) // 2
      break if @heap[parent].priority >= @heap[idx].priority
      @heap.swap(idx, parent)
      idx = parent
    end
  end

  private def heapify_down
    idx = 0
    while true
      largest = idx
      left  = 2 * idx + 1
      right = 2 * idx + 2

      largest = left  if left  < @heap.size && @heap[left ].priority > @heap[largest].priority
      largest = right if right < @heap.size && @heap[right].priority > @heap[largest].priority
      break if largest == idx
      @heap.swap(idx, largest)
      idx = largest
    end
  end
end

NEIGHBORS = [Point.new(0, 1), Point.new(0, -1), Point.new(1, 0), Point.new(-1, 0)]

def parse(input : String) : Tuple(Hash(String, Char), Set(Char), Point)
  grid = Hash(String, Char).new
  keys = Set(Char).new
  start = Point.new(0, 0)
  input.lines.each_with_index do |line, y|
    line.chomp.chars.each_with_index do |ch, x|
      pt = Point.new(x, y)
      grid[pt.to_s] = ch
      keys << ch if ch.ascii_lowercase?
      start = pt if ch == '@'
    end
  end
  {grid, keys, start}
end

def optimal_path(grid, all_keys, starts)
  pq = PriorityQueue.new
  dist = Hash(String, Int32).new

  s = Step.new(starts, 0, all_keys)
  starts.size.times do |i|
    ss = s.clone
    ss.worker_id = i
    pq.push(PriorityQueue::Item.new(ss, 0))
    dist[ss.to_s] = 0
  end

  until (cur = pq.pop?).nil?
    curr = cur.obj
    curdist = dist[curr.to_s]
    return curdist if curr.keys.empty?

    nextdist = curdist + 1
    NEIGHBORS.each do |n|
      np = Point.new(curr.p[curr.worker_id].x + n.x, curr.p[curr.worker_id].y + n.y)
      cell = grid[np.to_s]?
      next unless cell
      next if cell == '#'
      if cell.ascii_uppercase?
        needed = cell.downcase
        next if curr.keys.includes?(needed)
      end

      next_step = curr.clone
      next_step.p[curr.worker_id] = np
      found = false
      if cell.ascii_lowercase? && curr.keys.includes?(cell)
        found = true
        next_step.keys.delete(cell)
      end

      starts.size.times do |i|
        next if curr.worker_id != i && !found
        worker = next_step.clone
        worker.worker_id = i
        key = worker.to_s
        if !dist.has_key?(key) || nextdist < dist[key]
          dist[key] = nextdist
          pq.push(PriorityQueue::Item.new(worker, -nextdist))
        end
      end
    end
  end
  raise "No viable path"
end

input = File.read("input.txt")
grid, keys, start = parse(input)

grid[start.to_s] = '#'
NEIGHBORS.each { |n| grid[Point.new(start.x + n.x, start.y + n.y).to_s] = '#' }
starts = [
  Point.new(start.x - 1, start.y - 1),
  Point.new(start.x - 1, start.y + 1),
  Point.new(start.x + 1, start.y - 1),
  Point.new(start.x + 1, start.y + 1)
]
starts.each { |pt| grid[pt.to_s] = '@' }

puts optimal_path(grid, keys, starts)
