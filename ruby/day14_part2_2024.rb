
def mod(a, b)
  (a % b + b) % b
end

def parse_line(line)
  match = line.match(/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/)
  return nil unless match

  x, y, vx, vy = match[1..4].map(&:to_i)
  { x: x, y: y, vx: vx, vy: vy }
end

def move_robots(robots, size_x, size_y)
    robots.each do |robot|
        robot[:x] = mod(robot[:x] + robot[:vx], size_x)
        robot[:y] = mod(robot[:y] + robot[:vy], size_y)
    end
end

def count_quadrants(robots, size_x, size_y)
  counts = [0, 0, 0, 0]
  center_x = size_x / 2
  center_y = size_y / 2

  robots.each do |robot|
    x, y = robot[:x], robot[:y]
    if x < center_x
      if y < center_y
        counts[0] += 1
      elsif y > center_y
        counts[1] += 1
      end
    elsif x > center_x
      if y < center_y
        counts[2] += 1
      elsif y > center_y
        counts[3] += 1
      end
    end
  end
  counts
end

def has_no_overlaps(robots)
  positions = {}
  robots.each do |robot|
      pos = [robot[:x], robot[:y]]
      return false if positions[pos]
      positions[pos] = true
  end
  true
end

def draw_grid(robots, size_x, size_y)
    grid = Array.new(size_y) { Array.new(size_x, '.') }
    robots.each { |robot| grid[robot[:y]][robot[:x]] = '#' }
    grid.each { |row| puts row.join }
end


size_x = 101
size_y = 103
robots = []

File.foreach("input.txt") do |line|
    next if line.strip.empty?
    robot = parse_line(line)
    robots << robot if robot
end


robots_part1 = robots.map(&:clone)
100.times { move_robots(robots_part1, size_x, size_y) }
counts = count_quadrants(robots_part1, size_x, size_y)
safety_factor = counts.reduce(1, :*)
puts "Part 1 - Safety Factor after 100 seconds: #{safety_factor}"

robots_part2 = robots.map(&:clone)
seconds = 0
loop do
    break if has_no_overlaps(robots_part2)
    move_robots(robots_part2, size_x, size_y)
    seconds += 1
    if seconds > 1000000
        puts "Exceeded maximum iterations without finding a unique position configuration."
        exit 1
    end
end

puts "Part 2 - Fewest seconds to display Easter egg: #{seconds}"
puts "Final positions of robots:"
draw_grid(robots_part2, size_x, size_y)

