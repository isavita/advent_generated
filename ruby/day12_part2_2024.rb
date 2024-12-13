
def solve
  graph = File.read("input.txt").lines.map(&:chomp).reject(&:empty?).map(&:chars)
  h = graph.size
  w = graph[0].size
  moves = [[-1, 0], [0, -1], [1, 0], [0, 1]]
  sum = 0

  h.times do |y|
    w.times do |x|
      next if graph[y][x] == '.'
      area = 0
      target = graph[y][x]
      visited = {}
      side = {}

      search = lambda do |cx, cy, label|
        if graph[cy][cx] != target
          if label && !visited[[cx, cy]]
            save_outer(label, side, cx, cy)
          end
          return
        end

        visited[[cx, cy]] = true
        area += 1
        graph[cy][cx] = '.'

        moves.each_with_index do |(dx, dy), i|
          nx = cx + dx
          ny = cy + dy
          if nx < 0 || nx >= w || ny < 0 || ny >= h
            save_outer(i == 0 ? "left" : i == 1 ? "up" : i == 2 ? "right" : "down", side, nx, ny)
            next
          end
          search.call(nx, ny, i == 0 ? "left" : i == 1 ? "up" : i == 2 ? "right" : "down")
        end
      end

      search.call(x, y, nil)
      outer = count_outer(side)
      sum += area * outer
    end
  end
  puts sum
end

def save_outer(label, side, x, y)
  key = label == "up" || label == "down" ? "#{y}:#{x}" : "#{x}:#{y}"
  side[label] ||= {}
  side[label][key] = true
end

def count_outer(side)
  outer = 0
  side.each do |label, keys|
    sorted_keys = keys.keys.sort_by do |key|
      i, j = key.split(':').map(&:to_i)
      [i, j]
    end
    temp = []
    sorted_keys.each do |current|
      i, j = current.split(':').map(&:to_i)
      outer += 1 unless check(temp, i, j)
      temp << current
    end
  end
  outer
end

def check(ary, i, j)
  search = ["#{i}:#{j - 1}", "#{i}:#{j + 1}"]
  search.any? { |s| ary.include?(s) }
end

solve
