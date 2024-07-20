
class Machine
  attr_accessor :r

  def initialize
    @r = [0, 0, 0, 0]
  end

  def run(input)
    pos = 0
    input.each_char do |char|
      @r[0] = char.to_i
      pos += 1
      # Simulated operations based on input
      # Add your operations logic here
    end
  end
end

def read_input
  File.read("input.txt").lines.map(&:chomp)
end

def manual(s)
  k = [11, 14, 10, 14, -8, 14, -11, 10, -6, -9, 12, -5, -4, -9]
  l = [1, 1, 1, 1, 26, 1, 26, 1, 26, 26, 1, 26, 26, 26]
  m = [7, 8, 16, 8, 3, 12, 1, 8, 8, 14, 4, 14, 15, 6]
  w = s.chars.map(&:to_i)
  z = 0

  (0...14).each do |i|
    x = z % 26 + k[i]
    if l[i] == 1
      z = z * 26 + w[i] + m[i]
    else
      z /= 26
      z = z * 26 + w[i] + m[i] if x != w[i]
    end
  end
  z
end

def find_minimal_number
  l, k, m = [], [], []
  read_input.each_with_index do |line, i|
    case i % 18
    when 4 then l << line.split.last.to_i
    when 5 then k << line.split.last.to_i
    when 15 then m << line.split.last.to_i
    end
  end

  constraints = {}
  stack = []
  l.each_with_index do |val, i|
    if val == 1
      stack << i
    elsif val == 26
      pop = stack.pop
      constraints[pop] = [i, m[pop] + k[i]]
    end
  end

  min = Array.new(14, 0)
  constraints.each do |i, (j, offset)|
    vmin = 1
    vmin += 1 while vmin + offset < 1
    min[i] = vmin
    min[j] = vmin + offset
  end

  min.join.to_i
end

puts find_minimal_number
