
def main
  lines = File.read_lines("input.txt") rescue [] of String
  r = lines.size
  c = lines.map(&.size).max || 0
  if r == 0 || c == 0
    puts "Total rolls removed: 0"
    return
  end

  grid = lines.map(&.chars)
  dr = [-1, -1, -1, 0, 0, 1, 1, 1]
  dc = [-1, 0, 1, -1, 1, -1, 0, 1]

  removed = 0
  loop do
    changed = false
    r.times do |i|
      c.times do |j|
        next unless j < grid[i].size && grid[i][j] == '@'
        cnt = 0
        8.times do |k|
          nr = i + dr[k]
          nc = j + dc[k]
          if nr >= 0 && nr < r && nc >= 0 && nc < c
            next unless nc < grid[nr].size
            cnt += 1 if grid[nr][nc] == '@'
          end
        end
        if cnt < 4
          grid[i][j] = '*'
          changed = true
        end
      end
    end
    break unless changed
    r.times do |i|
      grid[i].each_with_index do |ch, j|
        if ch == '*'
          grid[i][j] = '.'
          removed += 1
        end
      end
    end
  end

  puts "Total rolls removed: #{removed}"
end

main
