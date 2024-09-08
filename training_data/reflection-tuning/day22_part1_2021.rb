def parse_step(line)
  state, coords = line.split
  state = (state == 'on')
  ranges = coords.split(',').map { |c| c.split('=')[1].split('..').map(&:to_i) }
  [state, ranges]
end

def within_init_region?(ranges)
  ranges.all? { |min, max| min >= -50 && max <= 50 }
end

def apply_step(grid, state, ranges)
  ranges.map! { |min, max| [[-50, min].max, [50, max].min] }
  (ranges[0][0]..ranges[0][1]).each do |x|
    (ranges[1][0]..ranges[1][1]).each do |y|
      (ranges[2][0]..ranges[2][1]).each do |z|
        grid[x+50][y+50][z+50] = state
      end
    end
  end
end

# Initialize 3D grid
grid = Array.new(101) { Array.new(101) { Array.new(101, false) } }

# Process input
File.readlines('input.txt').each do |line|
  state, ranges = parse_step(line.strip)
  apply_step(grid, state, ranges) if within_init_region?(ranges)
end

# Count on cubes
on_cubes = grid.flatten.count(true)

puts "Number of cubes on: #{on_cubes}"
