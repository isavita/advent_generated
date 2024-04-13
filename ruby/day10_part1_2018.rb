class Star
  attr_accessor :x, :y, :v_x, :v_y, :next

  def initialize(x, y, v_x, v_y)
    @x = x
    @y = y
    @v_x = v_x
    @v_y = v_y
    @next = nil
  end
end

def to_int(s)
  s.to_i
end

input = File.read("input.txt")
lines = input.split("\n")
head = Star.new(0, 0, 0, 0)
tail = head
re = /position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/

lines.each do |line|
  match = re.match(line)
  next if match.nil?
  star = Star.new(to_int(match[1]), to_int(match[2]), to_int(match[3]), to_int(match[4]))
  tail.next = star
  tail = star
end

smallest_t = 0
smallest_area = Float::INFINITY
100000.times do |t|
  max_x = max_y = -Float::INFINITY
  min_x = min_y = Float::INFINITY

  temp = head.next
  while temp
    x = temp.x + temp.v_x * t
    y = temp.y + temp.v_y * t
    max_x = [max_x, x].max
    min_x = [min_x, x].min
    max_y = [max_y, y].max
    min_y = [min_y, y].min
    temp = temp.next
  end

  len_x = max_x - min_x + 1
  len_y = max_y - min_y + 1
  area = len_x * len_y

  if smallest_area > area
    smallest_area = area
    smallest_t = t
  end
end

t = smallest_t
max_x = max_y = -Float::INFINITY
min_x = min_y = Float::INFINITY

temp = head.next
while temp
  temp.x += temp.v_x * t
  temp.y += temp.v_y * t
  max_x = [max_x, temp.x].max
  min_x = [min_x, temp.x].min
  max_y = [max_y, temp.y].max
  min_y = [min_y, temp.y].min
  temp = temp.next
end

mapper = Array.new(max_y - min_y + 1) { Array.new(max_x - min_x + 1, false) }

temp = head.next
while temp
  mapper[temp.y - min_y][temp.x - min_x] = true
  temp = temp.next
end

mapper.each do |row|
  puts row.map { |val| val ? "#" : " " }.join
end