
def read_input(file)
  File.readlines(file).map { |line| line.split(" -> ").map { |pair| pair.split(",").map(&:to_i) } }
end

def mark_lines(lines)
  grid = Hash.new(0)
  lines.each do |(x1, y1), (x2, y2)|
    if x1 == x2
      ([y1, y2].min..[y1, y2].max).each { |y| grid[[x1, y]] += 1 }
    elsif y1 == y2
      ([x1, x2].min..[x1, x2].max).each { |x| grid[[x, y1]] += 1 }
    end
  end
  grid
end

def count_overlaps(grid)
  grid.count { |_, v| v > 1 }
end

lines = read_input("input.txt")
grid = mark_lines(lines)
puts count_overlaps(grid)
