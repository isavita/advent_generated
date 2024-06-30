class BeamSimulator
  def initialize(grid)
    @grid = grid
    @height = grid.length
    @width = grid[0].length
  end

  def simulate(start_position, start_direction)
    energized = Set.new
    visited = Set.new
    beam_queue = [[start_position, start_direction]]

    until beam_queue.empty?
      position, direction = beam_queue.shift
      while in_bounds?(position) && !visited.include?([position, direction])
        energized.add(position)
        visited.add([position, direction])
        
        case @grid[position[0]][position[1]]
        when '/'
          direction = [-direction[1], -direction[0]]
        when '\\'
          direction = [direction[1], direction[0]]
        when '|'
          if direction[1] != 0  # Moving horizontally
            beam_queue << [position, [-1, 0]]  # Split upwards
            direction = [1, 0]  # Continue downwards
          end
        when '-'
          if direction[0] != 0  # Moving vertically
            beam_queue << [position, [0, -1]]  # Split left
            direction = [0, 1]  # Continue right
          end
        end
        
        position = [position[0] + direction[0], position[1] + direction[1]]
      end
    end

    energized.size
  end

  def find_max_energized
    max_energized = 0

    # Top edge, moving down
    @width.times do |x|
      max_energized = [max_energized, simulate([0, x], [1, 0])].max
    end

    # Bottom edge, moving up
    @width.times do |x|
      max_energized = [max_energized, simulate([@height - 1, x], [-1, 0])].max
    end

    # Left edge, moving right
    @height.times do |y|
      max_energized = [max_energized, simulate([y, 0], [0, 1])].max
    end

    # Right edge, moving left
    @height.times do |y|
      max_energized = [max_energized, simulate([y, @width - 1], [0, -1])].max
    end

    max_energized
  end

  private

  def in_bounds?(position)
    position[0].between?(0, @height - 1) && position[1].between?(0, @width - 1)
  end
end

def parse_input(input)
  input.split("\n").map(&:chars)
end

# Read input from file
input = File.read('input.txt')

# Parse input and run simulation
grid = parse_input(input)
simulator = BeamSimulator.new(grid)
max_energized = simulator.find_max_energized

puts "Maximum number of energized tiles: #{max_energized}"
