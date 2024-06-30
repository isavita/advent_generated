class PyroclasticFlow
  ROCKS = [
    [[0,0], [1,0], [2,0], [3,0]],           # ####
    [[1,0], [0,1], [1,1], [2,1], [1,2]],    # .#.
                                            # ###
                                            # .#.
    [[0,0], [1,0], [2,0], [2,1], [2,2]],    # ..#
                                            # ..#
                                            # ###
    [[0,0], [0,1], [0,2], [0,3]],           # #
                                            # #
                                            # #
                                            # #
    [[0,0], [1,0], [0,1], [1,1]]            # ##
                                            # ##
  ].freeze

  def initialize(jet_pattern)
    @jet_pattern = jet_pattern.chars
    @chamber = Array.new(7) { Array.new }
    @jet_index = 0
    @rock_index = 0
  end

  def simulate(num_rocks)
    num_rocks.times do
      drop_rock
    end
    @chamber.map(&:size).max
  end

  private

  def drop_rock
    rock = ROCKS[@rock_index].map { |x, y| [x + 2, y + @chamber.map(&:size).max + 3] }
    @rock_index = (@rock_index + 1) % ROCKS.size

    loop do
      # Apply jet
      jet = @jet_pattern[@jet_index]
      @jet_index = (@jet_index + 1) % @jet_pattern.size
      
      moved_rock = move_rock(rock, jet == '<' ? -1 : 1, 0)
      rock = moved_rock if valid_position?(moved_rock)

      # Move down
      moved_rock = move_rock(rock, 0, -1)
      if valid_position?(moved_rock)
        rock = moved_rock
      else
        place_rock(rock)
        break
      end
    end
  end

  def move_rock(rock, dx, dy)
    rock.map { |x, y| [x + dx, y + dy] }
  end

  def valid_position?(rock)
    rock.all? { |x, y| x.between?(0, 6) && y >= 0 && (@chamber[x][y] != '#' rescue true) }
  end

  def place_rock(rock)
    rock.each do |x, y|
      @chamber[x][y] = '#'
    end
  end
end

# Read input from file
jet_pattern = File.read('input.txt').strip

# Simulate and calculate height
simulator = PyroclasticFlow.new(jet_pattern)
height = simulator.simulate(2022)

puts "The tower of rocks will be #{height} units tall after 2022 rocks have stopped falling."
