
class Moon
  property x : Int32
  property y : Int32
  property z : Int32
  property vx : Int32
  property vy : Int32
  property vz : Int32

  def initialize(@x, @y, @z, @vx = 0, @vy = 0, @vz = 0)
  end

  def potential_energy
    @x.abs + @y.abs + @z.abs
  end

  def kinetic_energy
    @vx.abs + @vy.abs + @vz.abs
  end

  def total_energy
    potential_energy * kinetic_energy
  end
end

def parse_input(input)
  input.lines.map do |line|
    coords = line.scan(/-?\d+/).map(&.[0].to_i)
    Moon.new(coords[0], coords[1], coords[2])
  end
end

def apply_gravity(moons)
  moons.each_combination(2) do |pair|
    moon1, moon2 = pair
    
    # X-axis gravity
    if moon1.x < moon2.x
      moon1.vx += 1
      moon2.vx -= 1
    elsif moon1.x > moon2.x
      moon1.vx -= 1
      moon2.vx += 1
    end

    # Y-axis gravity
    if moon1.y < moon2.y
      moon1.vy += 1
      moon2.vy -= 1
    elsif moon1.y > moon2.y
      moon1.vy -= 1
      moon2.vy += 1
    end

    # Z-axis gravity
    if moon1.z < moon2.z
      moon1.vz += 1
      moon2.vz -= 1
    elsif moon1.z > moon2.z
      moon1.vz -= 1
      moon2.vz += 1
    end
  end
end

def update_positions(moons)
  moons.each do |moon|
    moon.x += moon.vx
    moon.y += moon.vy
    moon.z += moon.vz
  end
end

def solve_part1(moons, steps)
  steps.times do
    apply_gravity(moons)
    update_positions(moons)
  end

  moons.sum(&.total_energy)
end

def gcd(a : Int64, b : Int64)
  b == 0 ? a : gcd(b, a % b)
end

def lcm(a : Int64, b : Int64)
  a * b // gcd(a, b)
end

def find_cycle_length(moons, axis)
  initial_state = moons.map do |moon|
    case axis
    when :x
      {moon.x, moon.vx}
    when :y
      {moon.y, moon.vy}
    when :z
      {moon.z, moon.vz}
    end
  end

  steps = 0
  loop do
    apply_gravity(moons)
    update_positions(moons)
    steps += 1

    current_state = moons.map do |moon|
      case axis
      when :x
        {moon.x, moon.vx}
      when :y
        {moon.y, moon.vy}
      when :z
        {moon.z, moon.vz}
      end
    end

    break if current_state == initial_state
  end
  steps
end

def solve_part2(moons)
  x_cycle = find_cycle_length(moons.map(&.dup), :x)
  y_cycle = find_cycle_length(moons.map(&.dup), :y)
  z_cycle = find_cycle_length(moons.map(&.dup), :z)

  lcm(lcm(x_cycle.to_i64, y_cycle.to_i64), z_cycle.to_i64)
end

input = File.read("input.txt")
moons = parse_input(input)

puts "Part 1: #{solve_part1(moons.map(&.dup), 1000)}"
puts "Part 2: #{solve_part2(moons)}"
