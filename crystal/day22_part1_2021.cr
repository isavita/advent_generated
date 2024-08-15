require "file_utils"

class RebootStep
  property action : String
  property x_start : Int32
  property x_end : Int32
  property y_start : Int32
  property y_end : Int32
  property z_start : Int32
  property z_end : Int32

  def initialize(@action : String, @x_start : Int32, @x_end : Int32, @y_start : Int32, @y_end : Int32, @z_start : Int32, @z_end : Int32)
  end
end

def parse_reboot_step(line : String) : RebootStep
  parts = line.split(" ")
  action = parts[0]
  parts = parts[1].split(",")
  x_range, y_range, z_range = parts

  x_start, x_end = x_range[2..].split("..").map { |x| x.to_i }
  y_start, y_end = y_range[2..].split("..").map { |x| x.to_i }
  z_start, z_end = z_range[2..].split("..").map { |x| x.to_i }

  RebootStep.new(action, x_start, x_end, y_start, y_end, z_start, z_end)
end

def create_cube_grid(min_coord : Int32, max_coord : Int32) : Array(Array(Array(Bool)))
  grid_size = max_coord - min_coord + 1
  grid = Array.new(grid_size) do
    Array.new(grid_size) do
      Array.new(grid_size, false)
    end
  end
  grid
end

def execute_reboot_steps(cube_grid : Array(Array(Array(Bool))), reboot_steps : Array(RebootStep))
  reboot_steps.each do |step|
    next if !(step.x_start >= -50 && step.x_end <= 50 &&
              step.y_start >= -50 && step.y_end <= 50 &&
              step.z_start >= -50 && step.z_end <= 50)

    (step.x_start..step.x_end).each do |x|
      (step.y_start..step.y_end).each do |y|
        (step.z_start..step.z_end).each do |z|
          cube_grid[x + 50][y + 50][z + 50] = step.action == "on"
        end
      end
    end
  end
end

def count_on_cubes(cube_grid : Array(Array(Array(Bool)))) : Int32
  count = 0
  cube_grid.each do |plane|
    plane.each do |row|
      row.each do |cell|
        count += 1 if cell
      end
    end
  end
  count
end

def main
  reboot_steps = File.read_lines("input.txt").map do |line|
    parse_reboot_step(line)
  end

  min_coord, max_coord = -50, 50
  cube_grid = create_cube_grid(min_coord, max_coord)
  execute_reboot_steps(cube_grid, reboot_steps)
  on_cubes = count_on_cubes(cube_grid)

  puts on_cubes
end

main