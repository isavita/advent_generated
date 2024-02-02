
require 'set'

FAV_NUMBER = File.read('input.txt').to_i

def is_wall(x, y)
  sum = x*x + 3*x + 2*x*y + y + y*y + FAV_NUMBER
  sum.to_s(2).count('1').odd?
end

def valid_moves(x, y)
  moves = []
  moves << [x+1, y] unless x+1 < 0 || is_wall(x+1, y)
  moves << [x-1, y] unless x-1 < 0 || is_wall(x-1, y)
  moves << [x, y+1] unless y+1 < 0 || is_wall(x, y+1)
  moves << [x, y-1] unless y-1 < 0 || is_wall(x, y-1)
  moves
end

def bfs(start, max_steps)
  queue = [[start, 0]]
  visited = Set.new([start])
  count = 0

  while !queue.empty?
    (x, y), steps = queue.shift

    if steps <= max_steps
      count += 1
      valid_moves(x, y).each do |new_x, new_y|
        pos = [new_x, new_y]
        unless visited.include?(pos)
          visited.add(pos)
          queue << [pos, steps + 1]
        end
      end
    end
  end

  count
end

puts bfs([1, 1], 50)
