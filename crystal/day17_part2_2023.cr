
record Coord, x : Int32, y : Int32 do
  def +(other) : Coord
    Coord.new(x + other.x, y + other.y)
  end

  def -(other) : Coord
    Coord.new(x - other.x, y - other.y)
  end

  def opposite : Coord
    Coord.new(-x, -y)
  end
end

record Info, coord : Coord, dir : Coord, num_straight : Int32

class Grid
  property width : Int32, height : Int32, data : Hash(Coord, Int32)

  def initialize(@width, @height, @data)
  end

  def neighbors4(coord : Coord) : Array(Coord)
    dirs = [Coord.new(0, -1), Coord.new(-1, 0), Coord.new(0, 1), Coord.new(1, 0)]
    dirs.map { |d| coord + d }.select { |c| c.x >= 0 && c.x < width && c.y >= 0 && c.y < height }
  end

  def a_star_constrained(start : Coord, goal : Coord, min_straight : Int32, max_straight : Int32) : Int32
    start_info = Info.new(start, Coord.new(0, 0), 0)
    frontier = PriorityQueue(Info, Int32).new
    frontier.enqueue(start_info, 0)

    came_from = Hash(Info, Info).new
    cost_so_far = Hash(Info, Int32).new
    came_from[start_info] = start_info
    cost_so_far[start_info] = 0

    until frontier.empty?
      current = frontier.dequeue

      if current.coord == goal
        return cost_so_far[current]
      end

      neighbors4(current.coord).each do |next_coord|
        new_dir = next_coord - current.coord
        new_num_straight = new_dir == current.dir ? current.num_straight + 1 : 1

        next_info = Info.new(next_coord, new_dir, new_num_straight)
        new_cost = cost_so_far[current] + data[next_coord]

        lower_cost = !cost_so_far.has_key?(next_info) || new_cost < cost_so_far[next_info]
        valid_straight = (current.num_straight >= min_straight || new_dir == current.dir || current.coord == start) &&
                         (new_num_straight <= max_straight)
        not_opposite = new_dir != current.dir.opposite

        if lower_cost && valid_straight && not_opposite
          cost_so_far[next_info] = new_cost
          came_from[next_info] = current
          priority = new_cost + (next_coord.x - goal.x).abs + (next_coord.y - goal.y).abs
          frontier.enqueue(next_info, priority)
        end
      end
    end

    -1
  end
end

class PriorityQueue(K, V)
  include Enumerable(K)

  @heap : Array(Tuple(K, V))

  def initialize
    @heap = [] of Tuple(K, V)
  end

  def enqueue(key : K, priority : V)
    @heap << {key, priority}
    bubble_up(@heap.size - 1)
  end

  def dequeue : K
    swap(0, @heap.size - 1)
    result = @heap.pop[0]
    bubble_down(0) unless @heap.empty?
    result
  end

  def empty? : Bool
    @heap.empty?
  end

  def each(& : K -> _)
    @heap.each { |pair| yield pair[0] }
  end

  private def bubble_up(index : Int32)
    return if index == 0
    parent = (index - 1) // 2
    if @heap[index][1] < @heap[parent][1]
      swap(index, parent)
      bubble_up(parent)
    end
  end

  private def bubble_down(index : Int32)
    left = 2 * index + 1
    right = 2 * index + 2
    smallest = index

    smallest = left if left < @heap.size && @heap[left][1] < @heap[smallest][1]
    smallest = right if right < @heap.size && @heap[right][1] < @heap[smallest][1]

    if smallest != index
      swap(index, smallest)
      bubble_down(smallest)
    end
  end

  private def swap(i : Int32, j : Int32)
    @heap[i], @heap[j] = @heap[j], @heap[i]
  end
end

input = File.read_lines("input.txt")
grid_data = Hash(Coord, Int32).new
input.each_with_index do |line, y|
  line.chars.each_with_index do |char, x|
    grid_data[Coord.new(x, y)] = char.to_i
  end
end
grid = Grid.new(input[0].size, input.size, grid_data)

start = Coord.new(0, 0)
goal = Coord.new(grid.width - 1, grid.height - 1)
puts grid.a_star_constrained(start, goal, 4, 10)
