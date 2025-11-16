
class MinHeap
  def initialize
    @nodes = [] of State
  end

  def add(node : State)
    @nodes << node
    heapify_up(@nodes.size - 1)
  end

  def pop : State?
    return nil if @nodes.empty?
    @nodes.swap(0, @nodes.size - 1)
    root = @nodes.pop
    heapify_down(0)
    root
  end

  def empty?
    @nodes.empty?
  end

  private def heapify_up(i)
    while i > 0
      parent = (i - 1) // 2
      break if @nodes[i].energy >= @nodes[parent].energy
      @nodes.swap(i, parent)
      i = parent
    end
  end

  private def heapify_down(i)
    loop do
      smallest = i
      left  = 2*i + 1
      right = 2*i + 2
      smallest = left  if left  < @nodes.size && @nodes[left].energy  < @nodes[smallest].energy
      smallest = right if right < @nodes.size && @nodes[right].energy < @nodes[smallest].energy
      break if smallest == i
      @nodes.swap(i, smallest)
      i = smallest
    end
  end
end

struct Coord
  getter y : Int32, x : Int32
  def initialize(@y, @x); end
  def ==(other : Coord); @y == other.y && @x == other.x; end
  def hash; @y.hash ^ @x.hash; end
end

class State
  property grid : Array(Array(Char))
  property energy : Int32
  property path : String
  ROOM_X = {3, 5, 7, 9}
  WANT   = {'A', 'B', 'C', 'D'}
  COST   = {'A' => 1, 'B' => 10, 'C' => 100, 'D' => 1000}

  def initialize(@grid, @energy = 0, @path = "")
  end

  def done?
    ROOM_X.each_with_index do |x, i|
      2.upto(@grid.size - 2) { |y| return false if @grid[y][x] != WANT[i] }
    end
    true
  end

  def key
    @grid.map(&.join).join
  end

  def copy
    State.new(@grid.map(&.dup), @energy, @path)
  end

  def each_move
    each_amphipod do |c, ch|
      target_room = WANT.index(ch).not_nil!
      target_x    = ROOM_X[target_room]
      hallway     = c.y == 1

      if hallway
        next unless hallway_clear?(c.x, target_x)
        next unless room_ready?(target_x, ch)
        dest_y = room_bottom(target_x)
        energy = @energy + (c.y - 1 + (c.x - target_x).abs + (dest_y - 1)) * COST[ch]
        yield move(c, Coord.new(dest_y, target_x), energy)
      else
        next if c.y > 2 && @grid[c.y - 1][c.x] != '.'
        next if c.x == target_x && room_homelike?(target_x, ch)
        1.upto(11) do |x|
          next if ROOM_X.includes?(x)
          next unless hallway_clear?(c.x, x)
          energy = @energy + (c.y - 1 + (c.x - x).abs) * COST[ch]
          yield move(c, Coord.new(1, x), energy)
        end
      end
    end
  end

  private def each_amphipod
    @grid.each_with_index do |row, y|
      row.each_with_index do |ch, x|
        yield Coord.new(y, x), ch if COST.has_key?(ch)
      end
    end
  end

  private def hallway_clear?(from_x, to_x)
    range = from_x < to_x ? (from_x + 1)..to_x : to_x..(from_x - 1)
    range.all? { |x| @grid[1][x] == '.' }
  end

  private def room_ready?(x, ch)
    2.upto(@grid.size - 2) { |y| return false unless @grid[y][x] == '.' || @grid[y][x] == ch }
    true
  end

  private def room_homelike?(x, ch)
    2.upto(@grid.size - 2) { |y| return false unless @grid[y][x] == '.' || @grid[y][x] == ch }
    true
  end

  private def room_bottom(x)
    (@grid.size - 2).downto(2) { |y| return y if @grid[y][x] == '.' }
    0
  end

  private def move(from, to, energy)
    st = copy
    st.energy = energy
    st.grid[to.y][to.x] = st.grid[from.y][from.x]
    st.grid[from.y][from.x] = '.'
    st
  end
end

grid = File.read("input.txt").lines.map(&.chars)
heap = MinHeap.new
seen = Set(String).new
start = State.new(grid)
heap.add(start)

until heap.empty?
  cur = heap.pop.not_nil!
  next unless seen.add?(cur.key)
  if cur.done?
    puts cur.energy
    exit
  end
  cur.each_move { |st| heap.add(st) }
end
