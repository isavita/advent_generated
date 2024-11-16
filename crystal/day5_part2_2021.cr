
def abs(x)
  x < 0 ? -x : x
end

def sign(x)
  x > 0 ? 1 : x < 0 ? -1 : 0
end

lines = File.read_lines("input.txt").map do |line|
  parts = line.split(" -> ")
  start = parts[0].split(",").map(&.to_i)
  end_point = parts[1].split(",").map(&.to_i)
  [start[0], start[1], end_point[0], end_point[1]]
end

overlaps = Hash(Tuple(Int32, Int32), Int32).new(0)

lines.each do |line|
  x1, y1, x2, y2 = line

  x_step = sign(x2 - x1)
  y_step = sign(y2 - y1)
  steps = {abs(x2 - x1), abs(y2 - y1)}.max + 1

  steps.times do |i|
    point = {x1 + i * x_step, y1 + i * y_step}
    overlaps[point] += 1
  end
end

count = overlaps.count { |_, v| v > 1 }
puts count
