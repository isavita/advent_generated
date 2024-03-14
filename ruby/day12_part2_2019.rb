require 'bigdecimal'

class Vec3
  attr_accessor :x, :y, :z

  def initialize(x, y, z)
    @x, @y, @z = x, y, z
  end
end

class Moon
  attr_accessor :pos, :vel

  def initialize(pos, vel)
    @pos, @vel = pos, vel
  end
end

def apply_gravity(moons, axis)
  moons.each_with_index do |m1, i|
    moons.each_with_index do |m2, j|
      next if i >= j
      case axis
      when 'x'
        if m1.pos.x > m2.pos.x
          m1.vel.x -= 1
          m2.vel.x += 1
        elsif m1.pos.x < m2.pos.x
          m1.vel.x += 1
          m2.vel.x -= 1
        end
      when 'y'
        if m1.pos.y > m2.pos.y
          m1.vel.y -= 1
          m2.vel.y += 1
        elsif m1.pos.y < m2.pos.y
          m1.vel.y += 1
          m2.vel.y -= 1
        end
      when 'z'
        if m1.pos.z > m2.pos.z
          m1.vel.z -= 1
          m2.vel.z += 1
        elsif m1.pos.z < m2.pos.z
          m1.vel.z += 1
          m2.vel.z -= 1
        end
      end
    end
  end
end

def apply_velocity(moons, axis)
  moons.each do |m|
    case axis
    when 'x'
      m.pos.x += m.vel.x
    when 'y'
      m.pos.y += m.vel.y
    when 'z'
      m.pos.z += m.vel.z
    end
  end
end

def find_cycle(moons, initial_moons, axis)
  steps = 1
  loop do
    apply_gravity(moons, axis)
    apply_velocity(moons, axis)

    match = true
    moons.each_with_index do |m, i|
      case axis
      when 'x'
        match = false if m.pos.x != initial_moons[i].pos.x || m.vel.x != initial_moons[i].vel.x
      when 'y'
        match = false if m.pos.y != initial_moons[i].pos.y || m.vel.y != initial_moons[i].vel.y
      when 'z'
        match = false if m.pos.z != initial_moons[i].pos.z || m.vel.z != initial_moons[i].vel.z
      end
    end

    return steps if match
    steps += 1
  end
end

def lcm(a, b)
  a_big = BigDecimal(a)
  b_big = BigDecimal(b)
  (a_big * b_big) / BigDecimal((a_big.to_i.gcd(b_big.to_i)))
end

File.open('input.txt', 'r') do |file|
  moons = []
  initial_moons = []
  file.each_line do |line|
    x, y, z = line.scan(/-?\d+/).map(&:to_i)
    moons << Moon.new(Vec3.new(x, y, z), Vec3.new(0, 0, 0))
    initial_moons << Moon.new(Vec3.new(x, y, z), Vec3.new(0, 0, 0))
  end

  cycle_x = find_cycle(moons.dup, initial_moons.dup, 'x')
  cycle_y = find_cycle(moons.dup, initial_moons.dup, 'y')
  cycle_z = find_cycle(moons.dup, initial_moons.dup, 'z')

  lcm_xy = lcm(cycle_x, cycle_y)
  lcm_xyz = lcm(lcm_xy, cycle_z)

  puts lcm_xyz.to_i
end