require "big"

def floor_div(a : Int64, b : Int64) : Int64
  (a - a % b) // b
end

current = 50_i64
total = 0_i64

File.each_line("input.txt") do |line|
  line = line.strip
  next if line.empty?
  dir = line[0]
  dist = line[1..-1].to_i64
  if dir == 'R'
    nxt = current + dist
    total += floor_div(nxt, 100) - floor_div(current, 100)
    current = nxt
  else
    nxt = current - dist
    total += floor_div(current - 1, 100) - floor_div(nxt - 1, 100)
    current = nxt
  end
end

puts total