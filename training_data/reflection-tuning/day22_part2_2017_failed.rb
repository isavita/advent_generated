class VirusCarrier
  DIRECTIONS = [[-1, 0], [0, 1], [1, 0], [0, -1]]  # Up, Right, Down, Left
  
  def initialize(grid)
    @grid = grid
    @position = [grid.size / 2, grid.first.size / 2]
    @direction = 0  # Start facing up
    @infected_count = 0
  end
  
  def burst(part_two = false)
    current = @grid[@position[0]][@position[1]]
    
    if part_two
      case current
      when '.'  # Clean
        @direction = (@direction - 1) % 4
        @grid[@position[0]][@position[1]] = 'W'
      when 'W'  # Weakened
        @grid[@position[0]][@position[1]] = '#'
        @infected_count += 1
      when '#'  # Infected
        @direction = (@direction + 1) % 4
        @grid[@position[0]][@position[1]] = 'F'
      when 'F'  # Flagged
        @direction = (@direction + 2) % 4
        @grid[@position[0]][@position[1]] = '.'
      end
    else
      if current == '#'
        @direction = (@direction + 1) % 4
        @grid[@position[0]][@position[1]] = '.'
      else
        @direction = (@direction - 1) % 4
        @grid[@position[0]][@position[1]] = '#'
        @infected_count += 1
      end
    end
    
    @position[0] += DIRECTIONS[@direction][0]
    @position[1] += DIRECTIONS[@direction][1]
    
    # Expand grid if necessary
    expand_grid if @position.any? { |coord| coord < 0 || coord >= @grid.size }
  end
  
  def expand_grid
    new_size = @grid.size * 2
    new_grid = Array.new(new_size) { Array.new(new_size, '.') }
    
    @grid.each_with_index do |row, i|
      row.each_with_index do |cell, j|
        new_grid[i + @grid.size/2][j + @grid.size/2] = cell
      end
    end
    
    @grid = new_grid
    @position[0] += @grid.size / 4
    @position[1] += @grid.size / 4
  end
  
  def run(bursts, part_two = false)
    bursts.times { burst(part_two) }
    @infected_count
  end
end

# Read input
input = File.readlines('input.txt').map(&:strip)
grid = input.map { |line| line.chars }

# Part One
carrier = VirusCarrier.new(grid)
puts "Part One: #{carrier.run(10000)}"

# Part Two
carrier = VirusCarrier.new(grid)
puts "Part Two: #{carrier.run(10000000, true)}"
