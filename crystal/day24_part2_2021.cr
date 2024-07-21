
require "json"

class Machine
  @r : Array(Int32)

  def initialize
    @r = [0, 0, 0, 0]
  end

  def run(input : String, prog : String)
    pos = 0
    prog.split("\n").each do |line|
      f = line.split(" ")
      case f[0]
      when "inp"
        @r[reg(f[1])] = input[pos].to_i
        pos += 1
      when "add"
        @r[reg(f[1])] += get(f[2])
      when "mul"
        @r[reg(f[1])] *= get(f[2])
      when "div"
        @r[reg(f[1])] /= get(f[2])
      when "mod"
        @r[reg(f[1])] %= get(f[2])
      when "eql"
        @r[reg(f[1])] = @r[reg(f[1])] == get(f[2]) ? 1 : 0
      end
    end
  end

  def get(s : String) : Int32
    case s
    when "w", "x", "y", "z"
      @r[reg(s)]
    else
      s.to_i
    end
  end

  private def reg(r : String) : Int32
    r[0].ord - 'w'.ord
  end
end

def read_all(path : String) : String
  File.read(path).chomp
end

def manual(s : String) : Int32
  k = [11, 14, 10, 14, -8, 14, -11, 10, -6, -9, 12, -5, -4, -9]
  l = [1, 1, 1, 1, 26, 1, 26, 1, 26, 26, 1, 26, 26, 26]
  m = [7, 8, 16, 8, 3, 12, 1, 8, 8, 14, 4, 14, 15, 6]
  w = s.chars.map(&:to_i)
  z = 0

  (0...14).each do |i|
    x = z % 26 + k[i]
    if l[i] == 1
      z *= 26
      z += w[i] + m[i]
    else
      z /= 26
      if x != w[i]
        z *= 26
        z += w[i] + m[i]
      end
    end
  end
  z
end

k, l, m = [] of Int32, [] of Int32, [] of Int32
File.open("input.txt") do |file|
  file.each_line.with_index do |line, i|
    case i % 18
    when 4
      l << line.split.last.to_i
    when 5
      k << line.split.last.to_i
    when 15
      m << line.split.last.to_i
    end
  end
end

constraints = {} of Int32 => {Int32, Int32}
stack = [] of Int32

l.each_with_index do |value, i|
  case value
  when 1
    stack << i
  when 26
    pop = stack.pop
    constraints[pop] = {i, m[pop] + k[i]}
  end
end

min = Array(Int32).new(14, 0)
(0...14).each do |i|
  next unless constraints.has_key?(i)
  vmin = 1
  while vmin + constraints[i][1] < 1
    vmin += 1
  end
  min[i] = vmin
  min[constraints[i][0]] = vmin + constraints[i][1]
end

puts min.join.to_i
