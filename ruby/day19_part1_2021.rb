class Scanner
  attr_accessor :number, :x, :y, :z, :relative_coords, :rotations, :absolute_coords, :absolute_coords_map

  def initialize(number, coords)
    @number = number
    @x = 0
    @y = 0
    @z = 0
    @relative_coords = coords
    @absolute_coords = nil
    @absolute_coords_map = {}
    fill_rotations
  end

  def fill_absolute_coords_map
    @absolute_coords_map = {}
    @absolute_coords.each { |coord| @absolute_coords_map[coord] = true }
  end

  def fill_rotations
    pos_x = @relative_coords
    dir2 = pos_x.map { |x, y, z| [x, -y, -z] }
    dir3 = pos_x.map { |x, y, z| [x, -z, y] }
    dir4 = pos_x.map { |x, y, z| [-y, -z, x] }
    dir5 = pos_x.map { |x, y, z| [-x, -z, -y] }
    dir6 = pos_x.map { |x, y, z| [y, -z, -x] }

    six_rotations = [pos_x, dir2, dir3, dir4, dir5, dir6]

    @rotations = six_rotations.flat_map do |rotation|
      r2 = rotation.map { |x, y, z| [-y, x, z] }
      r3 = rotation.map { |x, y, z| [-x, -y, z] }
      r4 = rotation.map { |x, y, z| [y, -x, z] }
      [rotation, r2, r3, r4]
    end
  end
end

def find_absolute_coords_for_scanner(undet, settled)
  undet.rotations.each do |rotated_coords|
    settled.each do |set|
      set.absolute_coords.each do |abs_coord|
        rotated_coords.each do |rel_coord|
          unsettled_absolute_coords = make_absolute_coords_list(abs_coord, rel_coord, rotated_coords)

          matching_count = unsettled_absolute_coords.count { |ac| set.absolute_coords_map[ac] }

          if matching_count >= 12
            undet.relative_coords = rotated_coords
            undet.absolute_coords = unsettled_absolute_coords
            undet.fill_absolute_coords_map
            undet.x = abs_coord[0] - rel_coord[0]
            undet.y = abs_coord[1] - rel_coord[1]
            undet.z = abs_coord[2] - rel_coord[2]
            return undet, true
          end
        end
      end
    end
  end

  [undet, false]
end

def make_absolute_coords_list(absolute, relative, relative_coords)
  diff = [
    absolute[0] - relative[0],
    absolute[1] - relative[1],
    absolute[2] - relative[2]
  ]

  relative_coords.map do |c|
    [
      diff[0] + c[0],
      diff[1] + c[1],
      diff[2] + c[2]
    ]
  end
end

def parse_input(input)
  input.split("\n\n").map do |raw_scanner|
    lines = raw_scanner.split("\n")
    number = lines[0].scan(/\d+/).first.to_i
    coords = lines[1..-1].map { |line| line.split(',').map(&:to_i) }
    Scanner.new(number, coords)
  end
end

def solve(input)
  scanners = parse_input(input)

  settled = [scanners.shift]
  settled[0].absolute_coords = settled[0].relative_coords
  settled[0].fill_absolute_coords_map

  undetermined = scanners

  until undetermined.empty?
    undetermined.each_with_index do |undet, i|
      maybe_updated, ok = find_absolute_coords_for_scanner(undet, settled)
      if ok
        settled << maybe_updated
        undetermined.delete_at(i)
        break
      end
    end
  end

  all_beacons = {}
  settled.each do |s|
    s.absolute_coords_map.each_key { |c| all_beacons[c] = true }
  end

  all_beacons.size
end

# Read input from file
input = File.read('input.txt').strip

# Solve and print result
result = solve(input)
puts result
