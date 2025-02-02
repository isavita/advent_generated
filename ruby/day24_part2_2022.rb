#!/usr/bin/env ruby
require 'set'

def read_input(file)
  File.read(file).strip.split("\n")
end

lines = read_input("input.txt")
grid = {}
lines.each_with_index { |line, y| line.chars.each_with_index { |c, x| grid[[x,y]] = c if c != '.' } }
xs = grid.keys.map(&:first)
ys = grid.keys.map(&:last)
min_x, max_x = xs.min, xs.max + 1
min_y, max_y = ys.min, ys.max + 1
bounds = { min: [min_x, min_y], max: [max_x, max_y] }
inner_min = [bounds[:min][0] + 1, bounds[:min][1] + 1]
inner_max = [bounds[:max][0] - 1, bounds[:max][1] - 1]
inner_width = inner_max[0] - inner_min[0]
inner_height = inner_max[1] - inner_min[1]
period = inner_width.lcm(inner_height)

DIRS = { '^' => [0,-1], '>' => [1,0], 'v' => [0,1], '<' => [-1,0] }
MOVES = [[0,1],[0,-1],[1,0],[-1,0],[0,0]]

def in_bounds?(x,y, bounds)
  x >= bounds[:min][0] && x < bounds[:max][0] && y >= bounds[:min][1] && y < bounds[:max][1]
end

def steps(grid, bounds, inner_min, inner_width, inner_height, period, start, finish, init)
  q = [[start[0], start[1], init]]
  seen = Set.new
  while !q.empty?
    x, y, t = q.shift
    return t if [x, y] == finish
    MOVES.each do |dx, dy|
      nx, ny, nt = x + dx, y + dy, t + 1
      next unless in_bounds?(nx, ny, bounds)
      next if grid[[nx,ny]] == '#'
      if ny > 0 && ny < bounds[:max][1] - 1
        conflict = false
        DIRS.each do |sym, (mx,my)|
          rx = inner_min[0] + (((nx - mx * nt) - inner_min[0]) % inner_width)
          ry = inner_min[1] + (((ny - my * nt) - inner_min[1]) % inner_height)
          if grid[[rx,ry]] == sym
            conflict = true
            break
          end
        end
        next if conflict
      end
      key = [nx, ny, nt % period]
      next if seen.include?(key)
      seen.add(key)
      q << [nx, ny, nt]
    end
  end
  -1
end

entrance = [1, 0]
exit = [bounds[:max][0] - 2, bounds[:max][1] - 1]
first = steps(grid, bounds, inner_min, inner_width, inner_height, period, entrance, exit, 0)
second = steps(grid, bounds, inner_min, inner_width, inner_height, period, exit, entrance, first)
third = steps(grid, bounds, inner_min, inner_width, inner_height, period, entrance, exit, second)
puts third