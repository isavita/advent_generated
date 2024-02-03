
input = File.readlines('input.txt').map(&:chomp)

def biodiversity_rating(grid)
  bio_rating = 0
  grid.each_with_index do |row, i|
    row.chars.each_with_index do |char, j|
      bio_rating += 2**(i * 5 + j) if char == '#'
    end
  end
  bio_rating
end

def next_minute(grid)
  new_grid = Array.new(5) { '.' * 5 }

  grid.each_with_index do |row, i|
    row.chars.each_with_index do |char, j|
      bugs = 0
      [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |di, dj|
        ni, nj = i + di, j + dj
        next if ni < 0 || ni >= 5 || nj < 0 || nj >= 5

        bugs += 1 if grid[ni][nj] == '#'
      end

      if char == '#' && bugs != 1
        new_grid[i][j] = '.'
      elsif char == '.' && (bugs == 1 || bugs == 2)
        new_grid[i][j] = '#'
      else
        new_grid[i][j] = char
      end
    end
  end

  new_grid
end

seen = {}
while !seen.key?(input.join(''))
  seen[input.join('')] = true
  input = next_minute(input)
end

puts biodiversity_rating(input)
