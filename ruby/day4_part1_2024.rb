
def solve
  grid = File.readlines("input.txt").map(&:strip).reject(&:empty?)
  word = "XMAS"
  count = 0
  rows = grid.size
  cols = grid[0].size
  directions = [[0, 1], [1, 0], [1, 1], [-1, 1], [0, -1], [-1, 0], [-1, -1], [1, -1]]

  rows.times do |r|
    cols.times do |c|
      directions.each do |dr, dc|
        valid = true
        word.size.times do |i|
          nr, nc = r + dr * i, c + dc * i
          if nr < 0 || nr >= rows || nc < 0 || nc >= cols || grid[nr][nc] != word[i]
            valid = false
            break
          end
        end
        count += 1 if valid
      end
    end
  end
  puts "XMAS appears #{count} times in the word search"
end

solve
