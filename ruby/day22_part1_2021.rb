
reboot_steps = []

File.open("input.txt").each do |line|
  next if line.strip.empty?

  parts = line.split(" ")
  action = parts[0]
  parts = parts[1].split(",")
  x_range = parts[0][2..].split("..")
  y_range = parts[1][2..].split("..")
  z_range = parts[2][2..].split("..")

  x_start = x_range[0].to_i
  x_end = x_range[1].to_i
  y_start = y_range[0].to_i
  y_end = y_range[1].to_i
  z_start = z_range[0].to_i
  z_end = z_range[1].to_i

  reboot_steps << { action: action, x_start: x_start, x_end: x_end, y_start: y_start, y_end: y_end, z_start: z_start, z_end: z_end }
end

min_coord = -50
max_coord = 50
cube_grid = Array.new(max_coord - min_coord + 1) { Array.new(max_coord - min_coord + 1) { Array.new(max_coord - min_coord + 1, false) } }

reboot_steps.each do |step|
  next unless step[:x_start] >= -50 && step[:x_end] <= 50 && step[:y_start] >= -50 && step[:y_end] <= 50 && step[:z_start] >= -50 && step[:z_end] <= 50

  (step[:x_start]..step[:x_end]).each do |x|
    (step[:y_start]..step[:y_end]).each do |y|
      (step[:z_start]..step[:z_end]).each do |z|
        cube_grid[x + 50][y + 50][z + 50] = step[:action] == "on"
      end
    end
  end
end

on_cubes = cube_grid.flatten.count(true)
puts on_cubes
