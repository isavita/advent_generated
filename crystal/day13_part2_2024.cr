
struct Machine
  property ax : Int64
  property ay : Int64
  property bx : Int64
  property by : Int64
  property px : Int64
  property py : Int64

  def initialize(@ax, @ay, @bx, @by, @px, @py)
  end
end

def read_input(filename)
  machines = [] of Machine
  lines = [] of String
  File.each_line(filename) do |line|
    line = line.strip
    if line.empty?
      if !lines.empty?
        machines << parse_machine(lines)
        lines.clear
      end
    else
      lines << line
    end
  end
  if !lines.empty?
    machines << parse_machine(lines)
  end
  machines
end

def parse_machine(lines)
  ax, ay, bx, by, px, py = 0_i64, 0_i64, 0_i64, 0_i64, 0_i64, 0_i64
  lines.each do |line|
    line = line.gsub("Button A:", "A:").gsub("Button B:", "B:").gsub("Prize:", "P:")
    if line.starts_with?("A:")
      ax, ay = parse_line(line[2..])
    elsif line.starts_with?("B:")
      bx, by = parse_line(line[2..])
    elsif line.starts_with?("P:")
      px, py = parse_prize(line[2..])
    end
  end
  Machine.new(ax, ay, bx, by, px, py)
end

def parse_line(s)
  parts = s.strip.split(",")
  x = parts[0].gsub("X+", "").gsub("Y+", "").gsub("X=", "").gsub("Y=", "").strip.to_i64
  y = parts[1].gsub("X+", "").gsub("Y+", "").gsub("X=", "").gsub("Y=", "").strip.to_i64
  {x, y}
end

def parse_prize(s)
  parts = s.strip.split(",")
  x = parts[0].gsub("X=", "").strip.to_i64
  y = parts[1].gsub("Y=", "").strip.to_i64
  {x, y}
end

def solve_machine(ax, ay, bx, by, px, py)
  d = ax * by - ay * bx
  return -1_i64 if d == 0
  num_a = px * by - py * bx
  num_b = -px * ay + py * ax
  return -1_i64 if num_a % d != 0 || num_b % d != 0
  a = num_a // d
  b = num_b // d
  return -1_i64 if a < 0 || b < 0
  3_i64 * a + b
end

machines = read_input("input.txt")
results = [] of Int64
offset = 10000000000000_i64
machines.each do |m|
  cost = solve_machine(m.ax, m.ay, m.bx, m.by, m.px + offset, m.py + offset)
  results << cost if cost >= 0
end
if results.empty?
  puts "0 0"
else
  puts "#{results.size} #{results.sum}"
end
