Cuboid = Struct.new(:x1, :x2, :y1, :y2, :z1, :z2, :on)

def parse_input(input)
  input.map do |line|
    state, coords = line.split
    on = (state == 'on')
    ranges = coords.split(',').map { |c| c.split('=')[1].split('..').map(&:to_i) }
    Cuboid.new(*ranges.flatten, on)
  end
end

def intersect?(a, b)
  a.x1 <= b.x2 && b.x1 <= a.x2 &&
  a.y1 <= b.y2 && b.y1 <= a.y2 &&
  a.z1 <= b.z2 && b.z1 <= a.z2
end

def intersection(a, b)
  return nil unless intersect?(a, b)
  Cuboid.new(
    [a.x1, b.x1].max, [a.x2, b.x2].min,
    [a.y1, b.y1].max, [a.y2, b.y2].min,
    [a.z1, b.z1].max, [a.z2, b.z2].min,
    !a.on
  )
end

def volume(cuboid)
  return 0 if cuboid.x2 < cuboid.x1 || cuboid.y2 < cuboid.y1 || cuboid.z2 < cuboid.z1
  (cuboid.x2 - cuboid.x1 + 1) * (cuboid.y2 - cuboid.y1 + 1) * (cuboid.z2 - cuboid.z1 + 1)
end

def solve(steps)
  cuboids = []
  steps.each do |step|
    new_cuboids = []
    cuboids.each do |cuboid|
      intersection = intersection(cuboid, step)
      new_cuboids << intersection if intersection
    end
    cuboids.concat(new_cuboids)
    cuboids << step if step.on
  end

  cuboids.sum { |c| volume(c) * (c.on ? 1 : -1) }
end

# Read input
input = File.readlines('input.txt').map(&:strip)
steps = parse_input(input)

# Part 1: Initialize procedure region
init_region = Cuboid.new(-50, 50, -50, 50, -50, 50, true)
init_steps = steps.select { |step| intersect?(step, init_region) }
puts "Part 1: #{solve(init_steps)}"

# Part 2: Full reactor reboot
puts "Part 2: #{solve(steps)}"
