
struct Scanner
  property number : Int32
  property x, y, z : Int32
  property relative_coords : Array(Tuple(Int32, Int32, Int32))
  property rotations : Array(Array(Tuple(Int32, Int32, Int32)))
  property absolute_coords : Array(Tuple(Int32, Int32, Int32))
  property absolute_coords_map : Set(Tuple(Int32, Int32, Int32))

  def initialize(@number, @x = 0, @y = 0, @z = 0, @relative_coords = [] of Tuple(Int32, Int32, Int32), @absolute_coords = [] of Tuple(Int32, Int32, Int32))
    @absolute_coords_map = Set(Tuple(Int32, Int32, Int32)).new
    fill_rotations
  end

  def fill_absolute_coords_map
    @absolute_coords_map.clear
    @absolute_coords.each { |ac| @absolute_coords_map.add(ac) }
  end

  def fill_rotations
    pos_x = @relative_coords
    dir2 = [] of Tuple(Int32, Int32, Int32)
    dir3 = [] of Tuple(Int32, Int32, Int32)
    dir4 = [] of Tuple(Int32, Int32, Int32)
    dir5 = [] of Tuple(Int32, Int32, Int32)
    dir6 = [] of Tuple(Int32, Int32, Int32)
    pos_x.each do |c|
      x, y, z = c
      dir2 << {x, -y, -z}
      dir3 << {x, -z, y}
      dir4 << {-y, -z, x}
      dir5 << {-x, -z, -y}
      dir6 << {y, -z, -x}
    end
    six_rotations = [
      pos_x, dir2,
      dir3, dir4,
      dir5, dir6,
    ]

    final_rotations = [] of Array(Tuple(Int32, Int32, Int32))
    six_rotations.each do |rotation|
      r2 = [] of Tuple(Int32, Int32, Int32)
      r3 = [] of Tuple(Int32, Int32, Int32)
      r4 = [] of Tuple(Int32, Int32, Int32)
      rotation.each do |c|
        x, y, z = c
        r2 << {-y, x, z}
        r3 << {-x, -y, z}
        r4 << {y, -x, z}
      end
      final_rotations.concat([rotation, r2, r3, r4])
    end
    @rotations = final_rotations
  end
end

def find_absolute_coords_for_scanner(undet : Scanner, settled : Array(Scanner))
  undet.rotations.each do |rotated_coords|
    settled.each do |set|
      set.absolute_coords.each do |abs_coord|
        rotated_coords.each do |relative_coord|
          unsettled_absolute_coords = make_absolute_coords_list(abs_coord, relative_coord, rotated_coords)
          matching_count = 0
          unsettled_absolute_coords.each do |ac|
            matching_count += 1 if set.absolute_coords_map.includes?(ac)
          end
          if matching_count >= 12
            undet.relative_coords = rotated_coords
            undet.absolute_coords = unsettled_absolute_coords
            undet.fill_absolute_coords_map
            undet.x = abs_coord[0] - relative_coord[0]
            undet.y = abs_coord[1] - relative_coord[1]
            undet.z = abs_coord[2] - relative_coord[2]
            return undet, true
          end
        end
      end
    end
  end
  return undet, false
end

def make_absolute_coords_list(absolute : Tuple(Int32, Int32, Int32), relative : Tuple(Int32, Int32, Int32), relative_coords : Array(Tuple(Int32, Int32, Int32)))
  diff = {
    absolute[0] - relative[0],
    absolute[1] - relative[1],
    absolute[2] - relative[2],
  }
  abs_coords = [] of Tuple(Int32, Int32, Int32)
  relative_coords.each do |c|
    abs_coords << {
      diff[0] + c[0],
      diff[1] + c[1],
      diff[2] + c[2],
    }
  end
  abs_coords
end

def parse_input(input : String)
  scanners = [] of Scanner
  input.split("\n\n").each do |raw_scanner|
    lines = raw_scanner.split('\n')
    number = lines[0].split(" ")[2].to_i32
    coords = [] of Tuple(Int32, Int32, Int32)
    lines[1..].each do |line|
      x, y, z = line.split(',').map(&.to_i32)
      coords << {x, y, z}
    end
    sc = Scanner.new(number, relative_coords: coords)
    scanners << sc
  end
  scanners
end

def solve(input : String)
  scanners = parse_input(input)
  settled = [scanners[0]]
  settled[0].absolute_coords = settled[0].relative_coords
  settled[0].fill_absolute_coords_map
  undetermined = scanners[1..]
  until undetermined.empty?
    i = 0
    while i < undetermined.size
      maybe_updated, ok = find_absolute_coords_for_scanner(undetermined[i], settled)
      if ok
        settled << maybe_updated
        undetermined.delete_at(i)
        break
      end
      i += 1
    end
  end
  all_beacons = Set(Tuple(Int32, Int32, Int32)).new
  settled.each { |s| s.absolute_coords_map.each { |c| all_beacons.add(c) } }
  all_beacons.size
end

input = File.read("input.txt").strip
puts solve(input)
