
class Cuboid
  property x_range : Range(Int64, Int64)
  property y_range : Range(Int64, Int64)
  property z_range : Range(Int64, Int64)
  property on : Bool

  def initialize(@x_range, @y_range, @z_range, @on)
  end

  def volume
    (@x_range.end - @x_range.begin + 1) *
    (@y_range.end - @y_range.begin + 1) *
    (@z_range.end - @z_range.begin + 1)
  end

  def intersect(other : Cuboid)
    x_start = Math.max(@x_range.begin, other.x_range.begin)
    x_end = Math.min(@x_range.end, other.x_range.end)
    y_start = Math.max(@y_range.begin, other.y_range.begin)
    y_end = Math.min(@y_range.end, other.y_range.end)
    z_start = Math.max(@z_range.begin, other.z_range.begin)
    z_end = Math.min(@z_range.end, other.z_range.end)

    return nil if x_start > x_end || y_start > y_end || z_start > z_end

    Cuboid.new(
      x_start..x_end,
      y_start..y_end,
      z_start..z_end,
      !@on
    )
  end
end

def solve(input : Array(String), part2 : Bool = false)
  cuboids = [] of Cuboid
  input.each do |line|
    state, ranges = line.split
    x, y, z = ranges.split(',').map do |r|
      r.split('=')[1].split("..").map(&.to_i64)
    end

    cuboid = Cuboid.new(
      x[0]..x[1],
      y[0]..y[1],
      z[0]..z[1],
      state == "on"
    )

    next if !part2 && (
      cuboid.x_range.begin < -50 || cuboid.x_range.end > 50 ||
      cuboid.y_range.begin < -50 || cuboid.y_range.end > 50 ||
      cuboid.z_range.begin < -50 || cuboid.z_range.end > 50
    )

    new_cuboids = [] of Cuboid
    cuboids.each do |existing|
      if intersection = existing.intersect(cuboid)
        new_cuboids << intersection
      end
    end

    new_cuboids << cuboid if cuboid.on
    cuboids += new_cuboids
  end

  cuboids.sum { |c| c.on ? c.volume : -c.volume }
end

input = File.read_lines("input.txt")
puts "Part 1: #{solve(input)}"
puts "Part 2: #{solve(input, true)}"
