require "file_utils"

x = [1]
File.read_lines("input.txt").each do |line|
  case line
  when "noop"
    x << x.last
  else
    n = line.split.last.to_i
    x << x.last
    x << x.last + n
  end
end

grid = {} of Tuple(Int32, Int32) => Nil
x.each_with_index do |v, i|
  crtx, crty = i % 40, i // 40
  if (crtx - v).abs <= 1
    grid[{crtx, crty}] = nil
  else
    grid.delete({crtx, crty})
  end
end

6.times do |y|
  40.times do |x|
    print grid.has_key?({x, y}) ? "#" : "."
  end
  puts
end