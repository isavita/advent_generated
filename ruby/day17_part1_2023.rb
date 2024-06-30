require 'set'
require 'pqueue'

class CruciblePathFinder
  DIRECTIONS = [[0, 1], [1, 0], [0, -1], [-1, 0]]  # Right, Down, Left, Up

  def initialize(grid)
    @grid = grid
    @height = grid.length
    @width = grid[0].length
  end

  def find_minimum_heat_loss
    start = [0, 0, -1, 0]  # [row, col, direction, steps_in_direction]
    goal = [@height - 1, @width - 1]
    
    queue = PQueue.new([]) { |a, b| a[1] < b[1] }
    queue.push([start, 0, heuristic(start, goal)])
    
    came_from = {}
    cost_so_far = {start => 0}

    while !queue.empty?
      current, _ = queue.pop

      return cost_so_far[current] if current[0] == goal[0] && current[1] == goal[1]

      neighbors(current).each do |next_node|
        new_cost = cost_so_far[current] + @grid[next_node[0]][next_node[1]]

        if !cost_so_far.has_key?(next_node) || new_cost < cost_so_far[next_node]
          cost_so_far[next_node] = new_cost
          priority = new_cost + heuristic(next_node, goal)
          queue.push([next_node, priority])
          came_from[next_node] = current
        end
      end
    end

    nil  # No path found
  end

  private

  def neighbors(node)
    row, col, prev_dir, steps = node
    
    DIRECTIONS.each_with_index.flat_map do |dir, new_dir|
      next [] if (new_dir + 2) % 4 == prev_dir  # Can't reverse direction
      next [] if new_dir == prev_dir && steps == 3  # Can't go more than 3 steps in same direction

      new_row, new_col = row + dir[0], col + dir[1]
      next [] unless new_row.between?(0, @height - 1) && new_col.between?(0, @width - 1)

      new_steps = new_dir == prev_dir ? steps + 1 : 1
      [[new_row, new_col, new_dir, new_steps]]
    end
  end

  def heuristic(node, goal)
    (node[0] - goal[0]).abs + (node[1] - goal[1]).abs
  end
end

def parse_input(input)
  input.split("\n").map { |line| line.chars.map(&:to_i) }
end

# Read input from file
input = File.read('input.txt')

# Parse input and find minimum heat loss
grid = parse_input(input)
path_finder = CruciblePathFinder.new(grid)
min_heat_loss = path_finder.find_minimum_heat_loss

puts "Minimum heat loss: #{min_heat_loss}"
