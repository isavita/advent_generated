def read_all(path)
  File.read(path)
end

def abs(x)
  x < 0 ? -x : x
end

x = [1]
File.readlines("input.txt").each do |line|
  case line.strip
  when "noop"
    x << x.last
  else
    n = line.split(" ")[1].to_i
    x << x.last
    x << x.last + n
  end
end

grid = {}
x.each_with_index do |value, i|
  crtx, crty = i % 40, i / 40
  grid[[crtx, crty]] = true if abs(crtx - value) <= 1
end

(0..5).each do |y|
  (0..39).each do |x|
    print grid[[x, y]] ? "#" : "."
  end
  puts
end