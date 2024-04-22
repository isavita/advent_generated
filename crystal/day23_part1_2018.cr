class Nanobot
  property pos : Array(Int32), r : Int32

  def initialize(@pos, @r)
  end

  def distance(other : Nanobot) : Int32
    @pos.zip(other.pos).map { |a, b| (a - b).abs }.sum
  end

  def in_range?(other : Nanobot) : Bool
    distance(other) <= @r
  end
end

nanobots = File.read_lines("input.txt").map do |line|
  match = /pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)/.match(line)
  if match
    pos = [match[1].to_i, match[2].to_i, match[3].to_i]
    r = match[4].to_i
    Nanobot.new(pos, r)
  else
    raise "Invalid input: #{line}"
  end
end

strongest_nanobot = nanobots.max_by { |n| n.r }
in_range_count = nanobots.count { |n| strongest_nanobot.in_range?(n) }

puts in_range_count