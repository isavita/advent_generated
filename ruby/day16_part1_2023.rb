class BeamSimulator
  def initialize(grid)
    @grid = grid
    @height = grid.length
    @width = grid[0].length
    @energized = Set.new
    @visited = Set.new
  end

  def simulate
    beam_queue = [[[0, 0], [0, 1]]]  # Start at top-left, moving right

    until beam_queue.empty?
      position, direction = beam_queue.shift
      while in_bounds?(position) && !@visited.include?([position, direction])
        @energized.add(position)
        @visited.add([position, direction])
        
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

    @energized.size
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
energized_count = simulator.simulate

puts "Number of energized tiles: #{energized_count}"
