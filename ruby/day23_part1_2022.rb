#!/usr/bin/env ruby

Point = Struct.new(:x, :y)

class Elf
  attr_accessor :pos, :moving, :next_pos

  def initialize(pos)
    @pos = pos
    @moving = false
    @next_pos = nil
  end
end

N, E, S, W = 1, 3, 5, 7
MAP = {}
ELVES = []
ORDER = [N, S, W, E]
$curr_dir = 0  # Changed to a global variable
DIRS = [
  Point.new(-1, -1), # NW
  Point.new(-1, 0),  # N
  Point.new(-1, 1),  # NE
  Point.new(0, 1),   # E
  Point.new(1, 1),   # SE
  Point.new(1, 0),   # S
  Point.new(1, -1),  # SW
  Point.new(0, -1)   # W
]

def around_all_empty?(elf)
  DIRS.all? do |d|
    adj = Point.new(elf.pos.x + d.x, elf.pos.y + d.y)
    !MAP.key?(adj)
  end
end

def elf_in_direction?(elf, wanna_go)
  (-1..1).any? do |j|
    dxy = DIRS[(wanna_go + j + 8) % 8]
    adj = Point.new(elf.pos.x + dxy.x, elf.pos.y + dxy.y)
    MAP.key?(adj)
  end
end

def run
  proposes = Hash.new(0)

  ELVES.each do |elf|
    next if around_all_empty?(elf)

    4.times do |i|
      dir = ORDER[($curr_dir + i) % 4]
      next if elf_in_direction?(elf, dir)

      dxy = DIRS[dir]
      dest = Point.new(elf.pos.x + dxy.x, elf.pos.y + dxy.y)
      proposes[dest] += 1
      elf.next_pos = dest
      elf.moving = true
      break
    end
  end

  someone_moved = false
  ELVES.each do |elf|
    next unless elf.moving

    if proposes[elf.next_pos] > 1
      elf.moving = false
      next
    end

    someone_moved = true
    MAP.delete(elf.pos)
    MAP[elf.next_pos] = true
    elf.pos = elf.next_pos
    elf.moving = false
  end

  $curr_dir = ($curr_dir + 1) % 4
  someone_moved
end

def min_max
  min = Point.new(1 << 30, 1 << 30)
  max = Point.new(-(1 << 30), -(1 << 30))
  MAP.keys.each do |p|
    min.x = p.x if p.x < min.x
    min.y = p.y if p.y < min.y
    max.x = p.x if p.x > max.x
    max.y = p.y if p.y > max.y
  end
  [min, max]
end

# Parse input
File.readlines('input.txt', chomp: true).each_with_index do |line, row|
  line.chars.each_with_index do |char, col|
    if char == '#'
      p = Point.new(row, col)
      MAP[p] = true
      ELVES << Elf.new(p)
    end
  end
end

# Run simulation
10.times { run }

# Calculate result
min, max = min_max
count = 0
(min.x..max.x).each do |x|
  (min.y..max.y).each do |y|
    count += 1 unless MAP.key?(Point.new(x, y))
  end
end

puts count
