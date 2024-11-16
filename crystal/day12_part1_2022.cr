
class HillClimbing
  property grid : Array(Array(Char))
  property start : {Int32, Int32}
  property goal : {Int32, Int32}

  def initialize(input : Array(String))
    @grid = input.map(&.chars)
    @start = find_position('S')
    @goal = find_position('E')
    @grid[@start[0]][@start[1]] = 'a'
    @grid[@goal[0]][@goal[1]] = 'z'
  end

  def find_position(char : Char) : {Int32, Int32}
    @grid.each_with_index do |row, y|
      row.each_with_index do |cell, x|
        return {y, x} if cell == char
      end
    end
    raise "Position #{char} not found"
  end

  def valid_move?(current : {Int32, Int32}, next_pos : {Int32, Int32}) : Bool
    cy, cx = current
    ny, nx = next_pos
    return false if ny < 0 || ny >= @grid.size || nx < 0 || nx >= @grid[0].size

    current_height = @grid[cy][cx].ord
    next_height = @grid[ny][nx].ord
    next_height <= current_height + 1
  end

  def solve : Int32
    queue = [{@start, 0}]
    visited = Set({Int32, Int32}).new
    visited.add(@start)

    while !queue.empty?
      current, steps = queue.shift

      return steps if current == @goal

      [{0, 1}, {0, -1}, {1, 0}, {-1, 0}].each do |dy, dx|
        next_pos = {current[0] + dy, current[1] + dx}
        
        if valid_move?(current, next_pos) && !visited.includes?(next_pos)
          queue << {next_pos, steps + 1}
          visited.add(next_pos)
        end
      end
    end

    -1  # No path found
  end
end

# Read input from file
input = File.read_lines("input.txt")

# Solve the challenge
hill_climbing = HillClimbing.new(input)
puts hill_climbing.solve
