
require "set"

class MazeSolver
  def initialize(favorite_number : Int32)
    @favorite_number = favorite_number
  end

  def is_wall?(x : Int32, y : Int32) : Bool
    return true if x < 0 || y < 0

    value = x*x + 3*x + 2*x*y + y + y*y + @favorite_number
    bits = value.to_s(2).count('1')
    bits.odd?
  end

  def bfs(target_x : Int32, target_y : Int32, max_steps : Int32? = nil) : Int32
    queue = [{1, 1, 0}]
    visited = Set(Tuple(Int32, Int32)).new
    visited.add({1, 1})

    while !queue.empty?
      x, y, steps = queue.shift

      return steps if x == target_x && y == target_y && max_steps.nil?
      return visited.size if !max_steps.nil? && steps == max_steps

      [{0, 1}, {0, -1}, {1, 0}, {-1, 0}].each do |dx, dy|
        new_x, new_y = x + dx, y + dy

        if !is_wall?(new_x, new_y) && !visited.includes?({new_x, new_y})
          visited.add({new_x, new_y})
          queue << {new_x, new_y, steps + 1}
        end
      end
    end

    -1
  end
end

# Read input from file
favorite_number = File.read("input.txt").to_i

solver = MazeSolver.new(favorite_number)

# Part 1: Fewest steps to 31,39
part1_result = solver.bfs(31, 39)
puts "Part 1: #{part1_result}"

# Part 2: Locations reachable in 50 steps
part2_result = solver.bfs(0, 0, 50)
puts "Part 2: #{part2_result}"
