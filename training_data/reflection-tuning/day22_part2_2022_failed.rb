class CubeMap
  FACE_SIZE = 50
  DIRECTIONS = [[0, 1], [1, 0], [0, -1], [-1, 0]]  # Right, Down, Left, Up

  def initialize(map, instructions)
    @map = map
    @instructions = instructions
    @row, @col = find_start_position
    @facing = 0  # Start facing right
    @current_face = 1  # Start on face 1
  end

  def find_start_position
    @map[0].index('.'), 0
  end

  def solve
    @instructions.each do |instruction|
      if instruction.is_a?(Integer)
        move(instruction)
      else
        turn(instruction)
      end
    end
    calculate_password
  end

  def move(steps)
    steps.times do
      next_row, next_col, next_face, next_facing = next_position
      break if @map[next_row][next_col] == '#'
      @row, @col, @current_face, @facing = next_row, next_col, next_face, next_facing
    end
  end

  def next_position
    dr, dc = DIRECTIONS[@facing]
    next_row, next_col = @row + dr, @col + dc
    next_face = @current_face
    next_facing = @facing

    if out_of_bounds?(next_row, next_col)
      next_row, next_col, next_face, next_facing = wrap_around(next_row, next_col)
    end

    [next_row, next_col, next_face, next_facing]
  end

  def out_of_bounds?(row, col)
    row < 0 || row >= @map.size || col < 0 || col >= @map[row].size || @map[row][col] == ' '
  end

  def wrap_around(row, col)
    # Implement cube wrapping logic here
    # This is a placeholder and needs to be implemented based on the specific cube layout
    [row, col, @current_face, @facing]
  end

  def turn(direction)
    @facing = (direction == 'R') ? (@facing + 1) % 4 : (@facing - 1) % 4
  end

  def calculate_password
    1000 * (@row + 1) + 4 * (@col + 1) + @facing
  end
end

# Read input
map = []
File.foreach('input.txt') do |line|
  break if line.strip.empty?
  map << line.chomp
end

instructions = File.read('input.txt').split("\n\n").last.scan(/\d+|\D/).map { |x| x =~ /\d+/ ? x.to_i : x }

# Solve the puzzle
cube_map = CubeMap.new(map, instructions)
puts cube_map.solve
