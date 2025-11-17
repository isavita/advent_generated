
struct Map
  GEOLOGIC_Y = 16807
  GEOLOGIC_X = 48271
  CAVE_MOD   = 20183

  TOOL_NONE  = 1
  TOOL_TORCH = 2
  TOOL_GEAR  = 4

  def initialize(depth : Int32, tx : Int32, ty : Int32)
    @depth = depth
    @target = {tx, ty}
    @g_cache = Hash(Tuple(Int32, Int32), Int32).new
    @e_cache = Hash(Tuple(Int32, Int32), Int32).new
  end

  def geologic_index(x, y)
    key = {x, y}
    return @g_cache[key] if @g_cache.has_key?(key)

    idx = if x == 0 && y == 0
            0
          elsif x == @target[0] && y == @target[1]
            0
          elsif y == 0
            x * GEOLOGIC_Y
          elsif x == 0
            y * GEOLOGIC_X
          else
            erosion_level(x - 1, y) * erosion_level(x, y - 1)
          end
    @g_cache[key] = idx
  end

  def erosion_level(x, y)
    key = {x, y}
    return @e_cache[key] if @e_cache.has_key?(key)
    @e_cache[key] = (geologic_index(x, y) + @depth) % CAVE_MOD
  end

  def region_type(x, y)
    erosion_level(x, y) % 3
  end

  def allowed(t)
    case t
    when 0 then TOOL_GEAR | TOOL_TORCH
    when 1 then TOOL_GEAR | TOOL_NONE
    when 2 then TOOL_TORCH | TOOL_NONE
    else       raise "bad type"
    end
  end

  def neighbors(x, y, equip)
    n = [] of Tuple(Int32, Int32, Int32, Int32)
    [{1, 0}, {0, 1}, {-1, 0}, {0, -1}].each do |dx, dy|
      nx = x + dx
      ny = y + dy
      next if nx < 0 || ny < 0
      t = region_type(nx, ny)
      mask = allowed(t)
      next unless equip & mask != 0
      n << {nx, ny, equip, 1}
      n << {nx, ny, equip ^ mask, 8}
    end
    n
  end
end

lines = File.read("input.txt").lines
depth = lines[0].split(": ")[1].to_i
tx, ty = lines[1].split(": ")[1].split(",").map(&.to_i)

m = Map.new(depth, tx, ty)
target = {tx, ty}
queue = PriorityQueue(Tuple(Int32, Int32, Int32, Int32)).new
queue << {0, 0, 0, Map::TOOL_TORCH}
dist = Hash(Tuple(Int32, Int32, Int32), Int32).new
dist[{0, 0, Map::TOOL_TORCH}] = 0

best = 0
until queue.empty?
  time, x, y, equip = queue.pop
  if x == target[0] && y == target[1] && equip == Map::TOOL_TORCH
    best = time
    break
  end
  next if x > 8 * target[0] || y > 8 * target[1]
  next if time > dist[{x, y, equip}]
  m.neighbors(x, y, equip).each do |nx, ny, ne, cost|
    nt = time + cost
    key = {nx, ny, ne}
    if !dist.has_key?(key) || nt < dist[key]
      dist[key] = nt
      queue << {nt, nx, ny, ne}
    end
  end
end

puts best

class PriorityQueue(T)
  def initialize
    @heap = Array(T).new
  end

  def <<(item : T)
    @heap << item
    heap_up(@heap.size - 1)
  end

  def pop
    swap(0, @heap.size - 1)
    min = @heap.pop
    heap_down(0) unless @heap.empty?
    min
  end

  def empty?
    @heap.empty?
  end

  private def heap_up(i)
    return if i == 0
    parent = (i - 1) // 2
    if @heap[i][0] < @heap[parent][0]
      swap(i, parent)
      heap_up(parent)
    end
  end

  private def heap_down(i)
    l = 2 * i + 1
    r = 2 * i + 2
    smallest = i
    smallest = l if l < @heap.size && @heap[l][0] < @heap[smallest][0]
    smallest = r if r < @heap.size && @heap[r][0] < @heap[smallest][0]
    if smallest != i
      swap(i, smallest)
      heap_down(smallest)
    end
  end

  private def swap(i, j)
    @heap[i], @heap[j] = @heap[j], @heap[i]
  end
end
