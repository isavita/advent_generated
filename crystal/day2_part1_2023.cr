# Define a struct to store the counts of each color of cubes in a subset.
struct CubeSet
  property red : Int32
  property green : Int32
  property blue : Int32

  def initialize(@red = 0, @green = 0, @blue = 0)
  end
end

# Parse a line from the input file and return the game ID and an array of CubeSets.
def parse_line(line : String) : {Int32, Array(CubeSet)}
  # Extract the game ID
  id_str, cube_sets_str = line.split(": ")
  id = id_str.split.last.to_i

  # Split the cube sets and parse them
  cube_sets = cube_sets_str.split("; ").map do |subset|
    cubes = CubeSet.new
    subset.split(", ").each do |color_count|
      count, color = color_count.split
      case color
      when "red"
        cubes.red += count.to_i
      when "green"
        cubes.green += count.to_i
      when "blue"
        cubes.blue += count.to_i
      end
    end
    cubes
  end

  {id, cube_sets}
end

# Check if a game is possible with a given number of red, green, and blue cubes.
def game_possible?(cube_sets : Array(CubeSet), max_red : Int32, max_green : Int32, max_blue : Int32) : Bool
  cube_sets.all? do |subset|
    subset.red <= max_red && subset.green <= max_green && subset.blue <= max_blue
  end
end

# Main program logic starts here
max_red = 12
max_green = 13
max_blue = 14
total_id_sum = 0

# Read the input from the file and process each line
File.open("input.txt") do |file|
  file.each_line do |line|
    game_id, cube_sets = parse_line(line.strip)
    if game_possible?(cube_sets, max_red, max_green, max_blue)
      total_id_sum += game_id
    end
  end
end

# Print the answer to stdout
puts total_id_sum
