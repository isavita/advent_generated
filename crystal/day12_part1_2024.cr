
#!/usr/bin/env crystal

lines = File.read_lines("input.txt")
rows = lines.size
cols = rows > 0 ? lines[0].size : 0

visited = Array.new(rows) { Array.new(cols, false) }

dirs = [
  {-1, 0},
  {1, 0},
  {0, -1},
  {0, 1},
]

total = 0

rows.times do |r|
  cols.times do |c|
    next if visited[r][c]

    area = 0
    perimeter = 0
    char_val = lines[r][c]

    queue = [] of Tuple(Int32, Int32)
    head = 0
    queue << {r, c}
    visited[r][c] = true

    while head < queue.size
      x, y = queue[head]
      head += 1
      area += 1

      dirs.each do |dx, dy|
        nx = x + dx
        ny = y + dy

        if nx < 0 || nx >= rows || ny < 0 || ny >= cols
          perimeter += 1
        else
          if lines[nx][ny] != char_val
            perimeter += 1
          else
            unless visited[nx][ny]
              visited[nx][ny] = true
              queue << {nx, ny}
            end
          end
        end
      end
    end

    total += area * perimeter
  end
end

puts total
