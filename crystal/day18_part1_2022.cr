# Open the input file
input_file = File.open("input.txt")

# Initialize a set to store the cubes
cube_set = Set(Tuple(Int32, Int32, Int32)).new

# Read the input from file and store the cubes in the set
input_file.each_line do |line|
  cube = line.split(",").map(&.to_i)
  cube_set.add(Tuple.new(cube[0], cube[1], cube[2]))
end

# Initialize the surface area
surface_area = 0

# Iterate over each cube
cube_set.each do |cube|
  # Check each side of the cube
  [-1, 1].each do |dx|
    x, y, z = cube[0] + dx, cube[1], cube[2]
    surface_area += 1 if !cube_set.includes?(Tuple.new(x, y, z))
  end
  [-1, 1].each do |dy|
    x, y, z = cube[0], cube[1] + dy, cube[2]
    surface_area += 1 if !cube_set.includes?(Tuple.new(x, y, z))
  end
  [-1, 1].each do |dz|
    x, y, z = cube[0], cube[1], cube[2] + dz
    surface_area += 1 if !cube_set.includes?(Tuple.new(x, y, z))
  end
end

# Print the surface area
puts surface_area