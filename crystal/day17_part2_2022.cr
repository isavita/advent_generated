
CHAMBER_WIDTH = 7
PROFILE_DEPTH = 30

ROCK_SHAPES = [
  [{0, 0}, {1, 0}, {2, 0}, {3, 0}],
  [{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}],
  [{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}],
  [{0, 0}, {0, 1}, {0, 2}, {0, 3}],
  [{0, 0}, {1, 0}, {0, 1}, {1, 1}]
]

struct State
  property rock_index : Int32
  property jet_index : Int32
  property profile : Array(Int32)

  def initialize(@rock_index, @jet_index, @profile)
  end

  def_equals_and_hash @rock_index, @jet_index, @profile
end

def read_input(filename)
  File.read(filename).chomp
end

def try_move(rock, direction, chamber)
  moved = rock.map do |(x, y)|
    case direction
    when '<' then x -= 1
    when '>' then x += 1
    when 'v' then y -= 1
    end
    return [] of Tuple(Int32, Int64) if x < 0 || x >= CHAMBER_WIDTH || y < 1
    return [] of Tuple(Int32, Int64) if chamber.includes?({x, y})
    {x, y}
  end
  moved
end

def get_profile(chamber, highest)
  (0...CHAMBER_WIDTH).map do |x|
    y = highest
    while y >= Math.max(1_i64, highest - PROFILE_DEPTH)
      break chamber.includes?({x, y}) ? (highest - y).to_i : (y -= 1; next)
    end
    y < Math.max(1_i64, highest - PROFILE_DEPTH) ? PROFILE_DEPTH + 1 : (highest - y).to_i
  end
end

jet_pattern = read_input("input.txt")
total_rocks = 1_000_000_000_000_i64

chamber = Set(Tuple(Int32, Int64)).new
(0...CHAMBER_WIDTH).each { |x| chamber.add({x, 0_i64}) }

highest = 0_i64
jet_len = jet_pattern.size
jet_idx = 0_i64
rock_idx = 0

seen = Hash(State, Tuple(Int64, Int64)).new
extra = 0_i64
rock_num = 0_i64

while rock_num < total_rocks
  shape = ROCK_SHAPES[rock_idx % ROCK_SHAPES.size]
  rock = shape.map { |(dx, dy)| {2 + dx, highest + 4 + dy} }

  loop do
    dir = jet_pattern[jet_idx % jet_len]
    jet_idx += 1

    moved = try_move(rock, dir, chamber)
    rock = moved unless moved.empty?

    moved = try_move(rock, 'v', chamber)
    if moved.empty?
      rock.each do |(x, y)|
        chamber.add({x, y})
        highest = y if y > highest
      end
      break
    else
      rock = moved
    end
  end

  profile = get_profile(chamber, highest)
  state = State.new(rock_idx % ROCK_SHAPES.size, (jet_idx % jet_len).to_i32, profile)

  if seen.has_key?(state)
    prev_rock, prev_height = seen[state]
    cycle_len = rock_num - prev_rock
    cycle_height = highest - prev_height

    rem = total_rocks - rock_num
    if rem > 0
      cycles = rem // cycle_len
      extra += cycles * cycle_height
      rock_num += cycles * cycle_len
    end
  else
    seen[state] = {rock_num, highest}
  end

  rock_num += 1
  rock_idx += 1
end

puts highest + extra
