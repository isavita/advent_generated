class Solver
  DIRECTIONS = [[-1, 0], [1, 0], [0, -1], [0, 1]]

  def initialize(grid)
    @grid = grid
    @rows = grid.size
    @cols = grid[0].size
    @start = [0, @grid[0].index('.')]
    @end = [@rows - 1, @grid[-1].index('.')]
  end

  def solve(part2 = false)
    longest_path = 0
    stack = [[@start, Set.new([@start]), 0]]

    while !stack.empty?
      pos, visited, length = stack.pop

      if pos == @end
        longest_path = [longest_path, length].max
        next
      end

      neighbors(pos, part2).each do |nx, ny|
        new_pos = [nx, ny]
        unless visited.include?(new_pos)
          new_visited = visited.dup
          new_visited.add(new_pos)
          stack.push([new_pos, new_visited, length + 1])
        end
      end
    end

    longest_path
  end

  private

  def neighbors(pos, part2)
    x, y = pos
    neighbors = []

    DIRECTIONS.each do |dx, dy|
      nx, ny = x + dx, y + dy
      if valid?(nx, ny)
        if part2 || can_move?(x, y, nx, ny)
          neighbors << [nx, ny]
        end
      end
    end

    neighbors
  end

  def valid?(x, y)
    x.between?(0, @rows - 1) && y.between?(0, @cols - 1) && @grid[x][y] != '#'
  end

  def can_move?(x, y, nx, ny)
    case @grid[x][y]
    when '.' then true
    when '^' then nx == x - 1
    when 'v' then nx == x + 1
    when '<' then ny == y - 1
    when '>' then ny == y + 1
    else false
    end
  end
end

# Read input
grid = File.readlines('input.txt', chomp: true).map(&:chars)

solver = Solver.new(grid)
puts "Part 1: #{solver.solve}"
puts "Part 2: #{solver.solve(true)}"
