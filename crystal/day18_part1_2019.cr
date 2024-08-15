require "set"

struct Point
  property x, y

  def initialize(@x : Int32, @y : Int32)
  end
end

struct State
  property pos : Point
  property keys : Int32

  def initialize(@pos : Point, @keys : Int32)
  end

  def hash
    pos.hash ^ keys.hash
  end

  def ==(other : State)
    pos == other.pos && keys == other.keys
  end
end

def find_shortest_path(grid : Array(String), start : Point, key_map : Hash(Char, Int32)) : Int32
  dirs = [{0, -1}, {-1, 0}, {0, 1}, {1, 0}]
  visited = Set(State).new
  queue = [State.new(start, 0)]
  steps = 0

  while queue.size > 0
    size = queue.size
    size.times do
      current = queue.shift

      if current.keys == (1 << key_map.size) - 1
        return steps
      end

      dirs.each do |d|
        next_pos = Point.new(current.pos.x + d[0], current.pos.y + d[1])
        if next_pos.x >= 0 && next_pos.x < grid[0].size && next_pos.y >= 0 && next_pos.y < grid.size
          char = grid[next_pos.y][next_pos.x]
          if char != '#' && !(char.ascii_uppercase? && current.keys & (1 << key_map[char.downcase]) == 0)
            new_state = State.new(next_pos, current.keys)
            if char.ascii_lowercase?
              new_state.keys |= 1 << key_map[char]
            end
            unless visited.includes?(new_state)
              visited.add(new_state)
              queue << new_state
            end
          end
        end
      end
    end
    steps += 1
  end
  -1
end

file = File.open("input.txt")
grid = file.each_line.map { |line| line.chomp }.to_a # Convert the iterator to an array
start = Point.new(0, 0)
key_map = Hash(Char, Int32).new
key_counter = 0

grid.each_with_index do |line, y|
  line.each_char.with_index do |char, x|
    if char == '@'
      start = Point.new(x, y)
    elsif char.ascii_lowercase?
      key_map[char] = key_counter
      key_counter += 1
    end
  end
end

puts find_shortest_path(grid, start, key_map)