
require 'set'

def count_occupied_seats(seats)
  directions = [[-1, -1], [-1, 0], [-1, 1], [0, -1], [0, 1], [1, -1], [1, 0], [1, 1]]
  new_seats = seats.map(&:dup)
  changed = false

  seats.each_with_index do |row, i|
    row.each_with_index do |seat, j|
      next if seat == '.'

      occupied_adjacent = 0

      directions.each do |dir|
        x, y = i + dir[0], j + dir[1]

        while x >= 0 && x < seats.length && y >= 0 && y < row.length
          if seats[x][y] == '#'
            occupied_adjacent += 1
            break
          elsif seats[x][y] == 'L'
            break
          end

          x += dir[0]
          y += dir[1]
        end
      end

      if seat == 'L' && occupied_adjacent == 0
        new_seats[i][j] = '#'
        changed = true
      elsif seat == '#' && occupied_adjacent >= 5
        new_seats[i][j] = 'L'
        changed = true
      end
    end
  end

  return new_seats, changed
end

seats = File.readlines('input.txt', chomp: true)
seats = seats.map { |line| line.split('') }

loop do
  new_seats, changed = count_occupied_seats(seats)

  break unless changed

  seats = new_seats
end

puts seats.flatten.count('#')
