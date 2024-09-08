class Nanobot
  attr_reader :x, :y, :z, :r

  def initialize(x, y, z, r)
    @x, @y, @z, @r = x, y, z, r
  end

  def in_range?(other_x, other_y, other_z)
    manhattan_distance(other_x, other_y, other_z) <= @r
  end

  def manhattan_distance(other_x, other_y, other_z)
    (@x - other_x).abs + (@y - other_y).abs + (@z - other_z).abs
  end
end

def parse_input(input)
  input.map do |line|
    x, y, z, r = line.scan(/-?\d+/).map(&:to_i)
    Nanobot.new(x, y, z, r)
  end
end

def find_strongest_nanobot(nanobots)
  nanobots.max_by(&:r)
end

def count_nanobots_in_range(nanobots, strongest)
  nanobots.count { |bot| strongest.in_range?(bot.x, bot.y, bot.z) }
end

def find_optimal_position(nanobots)
  min_x, max_x = nanobots.map(&:x).minmax
  min_y, max_y = nanobots.map(&:y).minmax
  min_z, max_z = nanobots.map(&:z).minmax

  queue = [[[min_x, max_x], [min_y, max_y], [min_z, max_z]]]
  best_count = 0
  best_distance = Float::INFINITY

  while !queue.empty?
    ranges = queue.pop
    x_range, y_range, z_range = ranges

    mid_x = (x_range[0] + x_range[1]) / 2
    mid_y = (y_range[0] + y_range[1]) / 2
    mid_z = (z_range[0] + z_range[1]) / 2

    count = nanobots.count { |bot| bot.in_range?(mid_x, mid_y, mid_z) }
    distance = mid_x.abs + mid_y.abs + mid_z.abs

    if count > best_count || (count == best_count && distance < best_distance)
      best_count = count
      best_distance = distance
    end

    next if x_range[1] - x_range[0] <= 1 && y_range[1] - y_range[0] <= 1 && z_range[1] - z_range[0] <= 1

    [[x_range[0], mid_x], [mid_x + 1, x_range[1]]].product(
      [[y_range[0], mid_y], [mid_y + 1, y_range[1]]],
      [[z_range[0], mid_z], [mid_z + 1, z_range[1]]]
    ).each { |new_ranges| queue << new_ranges }
  end

  best_distance
end

# Main execution
nanobots = parse_input(File.readlines('input.txt', chomp: true))

# Part 1
strongest = find_strongest_nanobot(nanobots)
in_range_count = count_nanobots_in_range(nanobots, strongest)
puts "Part 1: #{in_range_count}"

# Part 2
optimal_distance = find_optimal_position(nanobots)
puts "Part 2: #{optimal_distance}"
