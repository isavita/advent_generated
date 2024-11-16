
class Moon
  property x : Int32
  property y : Int32
  property z : Int32
  property vx : Int32
  property vy : Int32
  property vz : Int32

  def initialize(@x : Int32, @y : Int32, @z : Int32)
    @vx = 0
    @vy = 0
    @vz = 0
  end

  def update_velocity(moons : Array(Moon))
    moons.each do |moon|
      @vx += moon.x <=> @x
      @vy += moon.y <=> @y
      @vz += moon.z <=> @z
    end
  end

  def update_position
    @x += @vx
    @y += @vy
    @z += @vz
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

def simulate_moons(moons : Array(Moon), steps : Int32)
  steps.times do
    # Update velocities
    moons.each do |moon|
      moon.update_velocity(moons.reject { |m| m == moon })
    end

    # Update positions
    moons.each(&.update_position)
  end

  moons.sum(&.total_energy)
end

# Read input from file
moons = File.read_lines("input.txt").map do |line|
  x, y, z = line.scan(/-?\d+/).map(&.[0].to_i)
  Moon.new(x, y, z)
end

# Simulate for 1000 steps and print total energy
puts simulate_moons(moons, 1000)
