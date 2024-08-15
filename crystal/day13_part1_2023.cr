class Mirror
  property rows : Array(Int32)
  property cols : Array(Int32)

  def initialize(rows : Array(Int32), cols : Array(Int32))
    @rows = rows
    @cols = cols
  end
end

def parse_input(input : Array(String)) : Array(Mirror)
  mirrors = [] of Mirror
  mirror_str = [] of String

  input.each do |line|
    if line.empty?
      mirrors << parse_mirror(mirror_str)
      mirror_str.clear
    else
      mirror_str << line
    end
  end
  mirrors << parse_mirror(mirror_str) unless mirror_str.empty?

  mirrors
end

def parse_mirror(mirror_str : Array(String)) : Mirror
  rows = Array(Int32).new(mirror_str.size, 0)
  cols = Array(Int32).new(mirror_str[0].size, 0)

  mirror_str.each_with_index do |line, y|
    line.each_char.with_index do |char, x|
      rows[y] <<= 1
      cols[x] <<= 1
      rows[y] += 1 if char == '#'
      cols[x] += 1 if char == '#'
    end
  end

  Mirror.new(rows, cols)
end

def get_mirror_axis(lines : Array(Int32)) : Int32
  (1...lines.size).each do |i|
    is_mirror = true
    (0...[i, lines.size - i].min).each do |j|
      if lines[i - 1 - j] != lines[i + j]
        is_mirror = false
        break
      end
    end
    return i if is_mirror
  end
  0
end

def solve(input : Array(String)) : Int32
  mirrors = parse_input(input)
  res = 0
  mirrors.each do |mirror|
    res += get_mirror_axis(mirror.cols)
    res += get_mirror_axis(mirror.rows) * 100
  end
  res
end

input = File.read("input.txt").split("\n").map(&.strip)
puts solve(input)