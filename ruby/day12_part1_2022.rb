require 'set'

class HillClimber
  def initialize(heightmap)
    @heightmap = heightmap
    @rows = heightmap.length
    @cols = heightmap[0].length
    find_start_and_end
  end

  def find_shortest_path
    queue = [[@start, 0]]
    visited = Set.new([@start])

    while !queue.empty?
      position, steps = queue.shift
      return steps if position == @end

      neighbors(position).each do |neighbor|
        if can_move?(position, neighbor) && !visited.include?(neighbor)
          queue << [neighbor, steps + 1]
          visited.add(neighbor)
        end
      end
    end

    nil  # No path found
  end

  private

  def find_start_and_end
    @heightmap.each_with_index do |row, i|
      row.each_with_index do |cell, j|
        @start = [i, j] if cell == 'S'
        @end = [i, j] if cell == 'E'
      end
    end
    @heightmap[@start[0]][@start[1]] = 'a'
    @heightmap[@end[0]][@end[1]] = 'z'
  end

  def neighbors(position)
    row, col = position
    [[row-1, col], [row+1, col], [row, col-1], [row, col+1]].select do |r, c|
      r.between?(0, @rows-1) && c.between?(0, @cols-1)
    end
  end

  def can_move?(from, to)
    from_height = @heightmap[from[0]][from[1]].ord
    to_height = @heightmap[to[0]][to[1]].ord
    to_height <= from_height + 1
  end
end

# Read input from file
input = File.readlines('input.txt', chomp: true).map(&:chars)

# Solve the problem
hill_climber = HillClimber.new(input)
steps = hill_climber.find_shortest_path

puts "Fewest steps required: #{steps}"
