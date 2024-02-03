
seats = File.readlines('input.txt').map(&:strip)

def adjacent_seats(seats, row, col)
  count = 0
  (-1..1).each do |i|
    (-1..1).each do |j|
      next if i == 0 && j == 0
      x = row + i
      y = col + j
      next if x < 0 || x >= seats.length || y < 0 || y >= seats[0].length
      count += 1 if seats[x][y] == '#'
    end
  end
  count
end

loop do
  new_seats = seats.map(&:dup)
  changed = false

  seats.each_with_index do |row, i|
    row.chars.each_with_index do |seat, j|
      next if seat == '.'

      occupied_adjacent = adjacent_seats(seats, i, j)

      if seat == 'L' && occupied_adjacent == 0
        new_seats[i][j] = '#'
        changed = true
      elsif seat == '#' && occupied_adjacent >= 4
        new_seats[i][j] = 'L'
        changed = true
      end
    end
  end

  break unless changed

  seats = new_seats
end

puts seats.join.count('#')
