
class P
  getter x : Int32
  getter y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

class Elf
  property pos : P
  property moving : Bool = false
  property next_pos : P?

  def initialize(@pos : P)
  end
end

DIRS = [
  P.new(-1, -1), # NW
  P.new(-1, 0),  # N
  P.new(-1, 1),  # NE
  P.new(0, 1),   # E
  P.new(1, 1),   # SE
  P.new(1, 0),   # S
  P.new(1, -1),  # SW
  P.new(0, -1)   # W
]

ORDER = [1, 5, 7, 3]

map = Hash(String, Bool).new
elves = [] of Elf
curr_dir = 0

parse = -> do
  File.read_lines("input.txt").each_with_index do |line, row|
    line.chars.each_with_index do |ch, col|
      if ch == '#'
        p = P.new(row, col)
        map["#{row},#{col}"] = true
        elves << Elf.new(p)
      end
    end
  end
end

around_all_empty = ->(elf : Elf) do
  DIRS.all? do |d|
    adj = P.new(elf.pos.x + d.x, elf.pos.y + d.y)
    !map.has_key?("#{adj.x},#{adj.y}")
  end
end

elf_in_direction = ->(elf : Elf, wanna_go : Int32) do
  (-1..1).any? do |j|
    dxy = DIRS[(wanna_go + j + 8) % 8]
    adj = P.new(elf.pos.x + dxy.x, elf.pos.y + dxy.y)
    map.has_key?("#{adj.x},#{adj.y}")
  end
end

run = -> do
  proposes = Hash(String, Int32).new(0)
  someone_moved = false

  elves.each do |e|
    if around_all_empty.call(e)
      next
    end

    4.times do |i|
      dir = ORDER[(curr_dir + i) % 4]
      next if elf_in_direction.call(e, dir)

      dxy = DIRS[dir]
      dest = P.new(e.pos.x + dxy.x, e.pos.y + dxy.y)
      proposes["#{dest.x},#{dest.y}"] += 1
      e.next_pos = dest
      e.moving = true
      break
    end
  end

  elves.each do |e|
    next unless e.moving
    dest_key = "#{e.next_pos.not_nil!.x},#{e.next_pos.not_nil!.y}"
    if proposes[dest_key] > 1
      e.moving = false
      next
    end

    someone_moved = true
    map.delete("#{e.pos.x},#{e.pos.y}")
    map[dest_key] = true
    e.pos = e.next_pos.not_nil!
    e.moving = false
  end

  curr_dir = (curr_dir + 1) % 4
  someone_moved
end

min_max = -> do
  min = P.new(Int32::MAX, Int32::MAX)
  max = P.new(Int32::MIN, Int32::MIN)
  map.keys.each do |key|
    x, y = key.split(',').map(&.to_i)
    min = P.new(x, min.y) if x < min.x
    min = P.new(min.x, y) if y < min.y
    max = P.new(x, max.y) if x > max.x
    max = P.new(max.x, y) if y > max.y
  end
  {min, max}
end

parse.call

10.times { run.call }

min, max = min_max.call

count = 0
(min.x..max.x).each do |x|
  (min.y..max.y).each do |y|
    count += 1 unless map.has_key?("#{x},#{y}")
  end
end

puts count
