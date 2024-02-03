
require 'matrix'

Neighbors8 = [
  Vector[0, 1], Vector[0, -1], Vector[1, 0], Vector[-1, 0],
  Vector[-1, -1], Vector[-1, 1], Vector[1, -1], Vector[1, 1]
]

class Part
  attr_accessor :xmin, :xmax, :y, :n

  def initialize(xmin, xmax, y, n)
    @xmin = xmin
    @xmax = xmax
    @y = y
    @n = n
  end

  def valid(grid)
    (@xmin..@xmax).each do |x|
      Neighbors8.each do |n|
        c = grid[n + Vector[x, @y]]
        return true if c && c != '.' && (c < '0' || c > '9')
      end
    end
    false
  end
end

input = File.read('input.txt').strip
grid = {}
parts = []
curr = nil

input.each_line.with_index do |line, y|
  if curr
    parts << curr
    curr = nil
  end
  line.chomp.each_char.with_index do |c, x|
    grid[Vector[x, y]] = c
    if c >= '0' && c <= '9'
      if curr.nil?
        curr = Part.new(x, x, y, c.to_i)
      else
        curr.n *= 10
        curr.n += c.to_i
        curr.xmax = x
      end
    elsif curr
      parts << curr
      curr = nil
    end
  end
end

parts_grid = {}
parts.each do |p|
  (p.xmin..p.xmax).each do |x|
    parts_grid[Vector[x, p.y]] = parts.index(p)
  end
end

sum = 0
grid.each do |p, c|
  if c == '*'
    neighbor_parts = {}
    Neighbors8.each do |n|
      i = parts_grid[n + p]
      neighbor_parts[i] = true if i
    end
    if neighbor_parts.size == 2
      prod = 1
      neighbor_parts.keys.each { |i| prod *= parts[i].n }
      sum += prod
    end
  end
end

puts sum
