require 'set'
require 'pqueue'

class Coord
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  def +(other)
    Coord.new(@x + other.x, @y + other.y)
  end

  def -(other)
    Coord.new(@x - other.x, @y - other.y)
  end

  def opposite
    Coord.new(-@x, -@y)
  end

  def ==(other)
    return false unless other.is_a?(Coord)
    @x == other.x && @y == other.y
  end

  alias eql? ==

  def hash
    [@x, @y].hash
  end
end

class Grid
  attr_accessor :width, :height, :data

  def initialize(input)
    @height = input.size
    @width = input[0].size
    @data = {}
    input.each_with_index do |line, y|
      line.chars.each_with_index do |char, x|
        @data[Coord.new(x, y)] = char.to_i
      end
    end
  end

  def neighbors4(coord)
    directions = [Coord.new(0, -1), Coord.new(-1, 0), Coord.new(0, 1), Coord.new(1, 0)]
    directions.map { |d| coord + d }.select { |n| in_bounds?(n) }
  end

  def in_bounds?(coord)
    coord.x >= 0 && coord.x < @width && coord.y >= 0 && coord.y < @height
  end
end

def heuristic(c1, c2)
  (c1.x - c2.x).abs + (c1.y - c2.y).abs
end

def a_star_constrained(grid, start, goal, min_straight, max_straight)
  frontier = PQueue.new { |a, b| a[:priority] < b[:priority] }
  frontier.push(coord: start, dir: nil, num_straight: 0, cost: 0, priority: heuristic(start, goal))
  cost_so_far = { [start, nil, 0] => 0 }

  while !frontier.empty?
    current = frontier.pop
    current_coord = current[:coord]
    current_dir = current[:dir]
    current_num_straight = current[:num_straight]
    current_cost = current[:cost]

    return current_cost if current_coord == goal

    grid.neighbors4(current_coord).each do |next_coord|
      new_dir = next_coord - current_coord
      new_num_straight = (new_dir == current_dir) ? current_num_straight + 1 : 1

      next_info = [next_coord, new_dir, new_num_straight]
      new_cost = current_cost + grid.data[next_coord]

      if (!cost_so_far.key?(next_info) || new_cost < cost_so_far[next_info]) &&
         (current_num_straight >= min_straight || new_dir == current_dir || current_coord == start) &&
         new_num_straight <= max_straight && new_dir != current_dir&.opposite

        cost_so_far[next_info] = new_cost
        frontier.push(coord: next_coord, dir: new_dir, num_straight: new_num_straight, cost: new_cost, priority: new_cost + heuristic(next_coord, goal))
      end
    end
  end

  -1
end

def solve(input)
  grid = Grid.new(input)
  start = Coord.new(0, 0)
  goal = Coord.new(grid.width - 1, grid.height - 1)
  a_star_constrained(grid, start, goal, 4, 10)
end

input = File.readlines("input.txt").map(&:chomp)
puts solve(input)