
record Coord, x : Int32, y : Int32 do
  def add(c2 : Coord)
    Coord.new(x + c2.x, y + c2.y)
  end

  def rotate90
    Coord.new(y, -x)
  end

  def rotate_neg90
    Coord.new(-y, x)
  end

  def in_bounds?(grid)
    0 <= x && x < grid.width && 0 <= y && y < grid.height
  end
end

record Grid, width : Int32, height : Int32, data : Hash(Coord, Char)

record Beam, origin : Coord, dir : Coord

NORTH = Coord.new(0, -1)
WEST = Coord.new(-1, 0)
SOUTH = Coord.new(0, 1)
EAST = Coord.new(1, 0)

def build_grid(input)
  data = Hash(Coord, Char).new
  input.each_with_index do |line, y|
    line.each_char_with_index do |char, x|
      data[Coord.new(x, y)] = char if char != '.'
    end
  end
  Grid.new(input[0].size, input.size, data)
end

def next_beam(grid, beam)
  beams = [] of Beam
  char = grid.data[beam.origin]?
  if char.nil?
    beams << Beam.new(beam.origin.add(beam.dir), beam.dir)
  else
    case char
    when '/'
      new_dir = (beam.dir == NORTH || beam.dir == SOUTH) ? beam.dir.rotate_neg90 : beam.dir.rotate90
      beams << Beam.new(beam.origin.add(new_dir), new_dir)
    when '\\'
      new_dir = (beam.dir == NORTH || beam.dir == SOUTH) ? beam.dir.rotate90 : beam.dir.rotate_neg90
      beams << Beam.new(beam.origin.add(new_dir), new_dir)
    when '|'
      if beam.dir == EAST || beam.dir == WEST
        beams << Beam.new(beam.origin.add(beam.dir.rotate90), beam.dir.rotate90)
        beams << Beam.new(beam.origin.add(beam.dir.rotate_neg90), beam.dir.rotate_neg90)
      else
        beams << Beam.new(beam.origin.add(beam.dir), beam.dir)
      end
    when '-'
      if beam.dir == NORTH || beam.dir == SOUTH
        beams << Beam.new(beam.origin.add(beam.dir.rotate90), beam.dir.rotate90)
        beams << Beam.new(beam.origin.add(beam.dir.rotate_neg90), beam.dir.rotate_neg90)
      else
        beams << Beam.new(beam.origin.add(beam.dir), beam.dir)
      end
    else
      beams << Beam.new(beam.origin.add(beam.dir), beam.dir)
    end
  end
  beams
end

def calculate_propagation(grid, start)
  already_seen = Set(Beam).new
  to_explore = [start]
  until to_explore.empty?
    beam = to_explore.shift
    if beam.origin.in_bounds?(grid) && !already_seen.includes?(beam)
      already_seen << beam
      to_explore.concat(next_beam(grid, beam))
    end
  end
  already_seen
end

def calculate_energization(already_seen)
  already_energized = Set(Coord).new
  already_seen.each { |beam| already_energized << beam.origin }
  already_energized
end

def solve(input)
  grid = build_grid(input)
  start = Beam.new(Coord.new(0, 0), EAST)
  already_seen = calculate_propagation(grid, start)
  already_energized = calculate_energization(already_seen)
  already_energized.size
end

input = File.read_lines("input.txt")
puts solve(input)
