
class HikingTrails
  @grid : Array(Array(Char))
  @rows : Int32
  @cols : Int32
  @start : {Int32, Int32}
  @goal : {Int32, Int32}

  def initialize(input : String)
    @grid = input.lines.map(&.chars)
    @rows = @grid.size
    @cols = @grid[0].size
    @start = find_start
    @goal = find_goal
  end

  def find_longest_hike
    visited = Set(Tuple(Int32, Int32)).new
    max_length = find_longest_path(@start[0], @start[1], visited, 0)
    max_length
  end

  private def find_longest_path(row : Int32, col : Int32, visited : Set(Tuple(Int32, Int32)), current_length : Int32) : Int32
    # Reached goal
    return current_length if {row, col} == @goal

    # Out of bounds or forest or already visited
    return 0 if !valid_position?(row, col) || visited.includes?({row, col})

    # Mark current position as visited
    visited.add({row, col})

    # Determine possible moves based on current tile
    max_length = 0
    case @grid[row][col]
    when '.'
      directions = [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
    when '^'
      directions = [{-1, 0}]
    when 'v'
      directions = [{1, 0}]
    when '<'
      directions = [{0, -1}]
    when '>'
      directions = [{0, 1}]
    else
      return 0
    end

    # Explore each possible direction
    directions.each do |dx, dy|
      next_row = row + dx
      next_col = col + dy
      path_length = find_longest_path(next_row, next_col, visited.dup, current_length + 1)
      max_length = Math.max(max_length, path_length)
    end

    # Backtrack
    visited.delete({row, col})
    max_length
  end

  private def valid_position?(row : Int32, col : Int32) : Bool
    row >= 0 && row < @rows && col >= 0 && col < @cols && @grid[row][col] != '#'
  end

  private def find_start : {Int32, Int32}
    @grid[0].each_with_index do |char, col|
      return {0, col} if char == '.'
    end
    raise "No start found"
  end

  private def find_goal : {Int32, Int32}
    @grid[@rows - 1].each_with_index do |char, col|
      return {@rows - 1, col} if char == '.'
    end
    raise "No goal found"
  end
end

# Read input from file
input = File.read("input.txt")

# Solve the hiking trail challenge
hiking_trails = HikingTrails.new(input)
longest_hike = hiking_trails.find_longest_hike

# Print the result
puts "Longest hike: #{longest_hike} steps"
