
require "file_utils"

module Day3
  extend self

  NEIGHBORS8 = [
    {0, 1}, {0, -1}, {1, 0}, {-1, 0},
    {-1, -1}, {-1, 1}, {1, -1}, {1, 1},
  ]

  record Part,
    xmin : Int32,
    xmax : Int32,
    y : Int32,
    n : Int32 do
    def valid(grid)
      (xmin..xmax).each do |x|
        NEIGHBORS8.each do |(dx, dy)|
          nx, ny = x + dx, y + dy
          if (c = grid[{nx, ny}]?) && c != '.' && !('0'..'9').includes?(c)
            return true
          end
        end
      end
      false
    end
  end

  def solve
    input = File.read("input.txt").strip
    grid = {} of {Int32, Int32} => Char
    parts = [] of Part
    curr = nil

    input.lines.each_with_index do |line, y|
      parts << curr if curr
      curr = nil

      line.chars.each_with_index do |c, x|
        grid[{x, y}] = c

        if ('0'..'9').includes?(c)
          if curr.nil?
            curr = Part.new(
              xmin: x,
              xmax: x,
              y: y,
              n: c.to_i
            )
          else
            curr = Part.new(
              xmin: curr.xmin,
              xmax: x,
              y: y,
              n: curr.n * 10 + c.to_i
            )
          end
        elsif !curr.nil?
          parts << curr
          curr = nil
        end
      end
    end

    parts << curr if curr

    parts_grid = {} of {Int32, Int32} => Int32
    parts.each_with_index do |p, i|
      (p.xmin..p.xmax).each do |x|
        parts_grid[{x, p.y}] = i
      end
    end

    sum = 0
    grid.each do |(x, y), c|
      if c == '*'
        neighbor_parts = Set(Int32).new
        NEIGHBORS8.each do |(dx, dy)|
          nx, ny = x + dx, y + dy
          if (i = parts_grid[{nx, ny}]?)
            neighbor_parts << i
          end
        end

        if neighbor_parts.size == 2
          prod = neighbor_parts.map { |i| parts[i].n }.product
          sum += prod
        end
      end
    end

    sum
  end
end

puts Day3.solve
