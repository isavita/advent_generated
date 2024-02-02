input = File.read("input.txt").to_i

def is_wall(x, y, input)
  sum = x*x + 3*x + 2*x*y + y + y*y + input
  bits = sum.to_s(2).count("1")
  bits.odd?
end

def bfs(input)
  queue = [[1, 1, 0]]
  visited = {}

  while !queue.empty?
    x, y, steps = queue.shift

    if x == 31 && y == 39
      return steps
    end

    if x >= 0 && y >= 0 && !is_wall(x, y, input) && !visited[[x, y]]
      visited[[x, y]] = true
      queue << [x+1, y, steps+1]
      queue << [x-1, y, steps+1]
      queue << [x, y+1, steps+1]
      queue << [x, y-1, steps+1]
    end
  end
end

puts bfs(input)