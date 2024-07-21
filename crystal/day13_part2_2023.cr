
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
      break unless is_mirror
      is_mirror = lines[i - 1 - j] == lines[i + j]
    end

    return i if is_mirror
  end
  0
end

def get_mirror_axis_with_one_smudge(lines : Array(Int32)) : Int32
  (1...lines.size).each do |i|
    is_mirror = true
    num_smudges = 0

    (0...[i, lines.size - i].min).each do |j|
      break unless is_mirror
      if lines[i - 1 - j] != lines[i + j]
        if num_smudges > 0
          is_mirror = false
        else
          dif = lines[i - 1 - j] ^ lines[i + j]
          is_only_one_smudge = (dif & (dif - 1)) == 0
          num_smudges += 1 if is_only_one_smudge
          is_mirror = false unless is_only_one_smudge
        end
      end
    end

    return i if is_mirror && num_smudges == 1
  end
  0
end

def solve(input : Array(String)) : Int32
  mirrors = parse_input(input)
  res = 0

  mirrors.each do |mirror|
    res += get_mirror_axis_with_one_smudge(mirror.cols)
    res += get_mirror_axis_with_one_smudge(mirror.rows) * 100
  end

  res
end

def read_file(file_name : String) : Array(String)
  File.read(file_name).split("\n").map(&.strip)
end

input = read_file("input.txt")
puts solve(input)
