
require "set"

def mod(a, b)
  (a % b + b) % b
end

def parse_line(line)
  m = line.match(/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/)
  raise "Invalid line format: #{line}" unless m
  {x: m[1].to_i, y: m[2].to_i, vx: m[3].to_i, vy: m[4].to_i}
end

def move_robots(robots, sx, sy)
  robots.map do |r|
    {x: mod(r[:x] + r[:vx], sx), y: mod(r[:y] + r[:vy], sy), vx: r[:vx], vy: r[:vy]}
  end
end

def count_quadrants(robots, sx, sy)
  c = [0, 0, 0, 0]
  cx = sx // 2
  cy = sy // 2
  robots.each do |r|
    x = r[:x]
    y = r[:y]
    if x < cx
      if y < cy
        c[0] += 1
      elsif y > cy
        c[1] += 1
      end
    elsif x > cx
      if y < cy
        c[2] += 1
      elsif y > cy
        c[3] += 1
      end
    end
  end
  c
end

def has_no_overlaps(robots)
  set = Set(Tuple(Int32, Int32)).new
  robots.each do |r|
    pos = {r[:x], r[:y]}
    return false if set.includes?(pos)
    set.add(pos)
  end
  true
end

def draw_grid(robots, sx, sy)
  grid = Array.new(sy) { Array.new(sx, '.') }
  robots.each { |r| grid[r[:y]][r[:x]] = '#' }
  grid.each { |row| puts row.join }
end

def main
  sx = 101
  sy = 103
  robots = File.read_lines("input.txt").map { |l| parse_line(l.strip) }

  part1 = robots.dup
  100.times { part1 = move_robots(part1, sx, sy) }
  counts = count_quadrants(part1, sx, sy)
  safety = counts.reduce(1) { |a, b| a * b }
  puts "Part 1 - Safety Factor after 100 seconds: #{safety}"

  part2 = robots.dup
  seconds = 0
  loop do
    break if has_no_overlaps(part2)
    part2 = move_robots(part2, sx, sy)
    seconds += 1
    if seconds > 1_000_000
      puts "Exceeded maximum iterations without finding a unique position configuration."
      return
    end
  end
  puts "Part 2 - Fewest seconds to display Easter egg: #{seconds}"
  puts "Final positions of robots:"
  draw_grid(part2, sx, sy)
end

main
