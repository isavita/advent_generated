
record Nanobot, x : Int32, y : Int32, z : Int32, r : Int32

def parse_input(file : String) : Array(Nanobot)
  File.read_lines(file).map do |line|
    m = /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/.match(line).not_nil!
    Nanobot.new(m[1].to_i, m[2].to_i, m[3].to_i, m[4].to_i)
  end
end

def manhattan(a, b)
  (a[0] - b[0]).abs + (a[1] - b[1]).abs + (a[2] - b[2]).abs
end

def part1(bots)
  s = bots.max_by(&.r)
  bots.count { |b| manhattan({s.x, s.y, s.z}, {b.x, b.y, b.z}) <= s.r }
end

def min_dist(x, y, z, size)
  dx = x > 0 ? x : (x + size - 1 < 0 ? -(x + size - 1) : 0)
  dy = y > 0 ? y : (y + size - 1 < 0 ? -(y + size - 1) : 0)
  dz = z > 0 ? z : (z + size - 1 < 0 ? -(z + size - 1) : 0)
  dx + dy + dz
end

def part2(bots)
  min_x, max_x = bots.min_of(&.x), bots.max_of(&.x)
  min_y, max_y = bots.min_of(&.y), bots.max_of(&.y)
  min_z, max_z = bots.min_of(&.z), bots.max_of(&.z)

  size = 1
  while size < ({max_x - min_x, max_y - min_y, max_z - min_z}.max)
    size *= 2
  end

  heap = PriorityQueue(Tuple(Int32, Int32, Int32, Int32, Int32, Int32)).new
  heap.push({0, min_dist(min_x, min_y, min_z, size), size, min_x, min_y, min_z})

  best_dist = 0
  best_count = -1

  until heap.empty?
    neg_count, dist, size, x, y, z = heap.pop
    count = -neg_count

    if size == 1
      if count > best_count || (count == best_count && dist < best_dist)
        best_count = count
        best_dist = dist
      end
      return best_dist
    end

    half = size // 2
    {0, half}.each do |dx|
      {0, half}.each do |dy|
        {0, half}.each do |dz|
          nx, ny, nz = x + dx, y + dy, z + dz
          new_size = half < 1 ? 1 : half

          cnt = 0
          bots.each do |b|
            d = 0
            d += nx - b.x if b.x < nx
            d += b.x - (nx + new_size - 1) if b.x > nx + new_size - 1
            d += ny - b.y if b.y < ny
            d += b.y - (ny + new_size - 1) if b.y > ny + new_size - 1
            d += nz - b.z if b.z < nz
            d += b.z - (nz + new_size - 1) if b.z > nz + new_size - 1
            cnt += 1 if d <= b.r
          end

          heap.push({-cnt, min_dist(nx, ny, nz, new_size), new_size, nx, ny, nz})
        end
      end
    end
  end
  best_dist
end

bots = parse_input("input.txt")
puts part1(bots)
puts part2(bots)

# simple priority queue
class PriorityQueue(T)
  def initialize
    @heap = [] of T
  end

  def push(item : T)
    @heap << item
    heap_up(@heap.size - 1)
  end

  def pop : T
    swap(0, @heap.size - 1)
    item = @heap.pop
    heap_down(0) if @heap.size > 0
    item
  end

  def empty?
    @heap.empty?
  end

  private def heap_up(i)
    while i > 0
      p = (i - 1) // 2
      break if cmp(p, i) <= 0
      swap(p, i)
      i = p
    end
  end

  private def heap_down(i)
    while true
      l = 2*i + 1
      r = 2*i + 2
      smallest = i
      smallest = l if l < @heap.size && cmp(l, smallest) < 0
      smallest = r if r < @heap.size && cmp(r, smallest) < 0
      break if smallest == i
      swap(i, smallest)
      i = smallest
    end
  end

  private def cmp(a, b)
    # min-heap by first element
    @heap[a][0] <=> @heap[b][0]
  end

  private def swap(a, b)
    @heap[a], @heap[b] = @heap[b], @heap[a]
  end
end
