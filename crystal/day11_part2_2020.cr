class SeatingSystem
  property grid : Array(Array(Char))

  def initialize(input : String)
    @grid = input.lines.map(&.chars.to_a)
  end

  def adjacent_seats(x : Int, y : Int) : Int
    count = 0
    (-1..1).each do |dx|
      (-1..1).each do |dy|
        next if dx == 0 && dy == 0
        nx, ny = x + dx, y + dy
        while nx >= 0 && nx < grid.size && ny >= 0 && ny < grid[0].size
          case grid[nx][ny]
          when 'L' then break
          when '#' then count += 1; break
          when '.' then nx += dx; ny += dy; next
          end
          break
        end
      end
    end
    count
  end

  def apply_rules
    new_grid = grid.map(&.dup)
    grid.each_with_index do |row, x|
      row.each_with_index do |seat, y|
        case seat
        when 'L'
          if adjacent_seats(x, y) == 0
            new_grid[x][y] = '#'
          end
        when '#'
          if adjacent_seats(x, y) >= 5
            new_grid[x][y] = 'L'
          end
        end
      end
    end
    @grid = new_grid
  end

  def stabilize
    loop do
      new_grid = grid.map(&.dup)
      changed = false
      grid.each_with_index do |row, x|
        row.each_with_index do |seat, y|
          case seat
          when 'L'
            if adjacent_seats(x, y) == 0
              new_grid[x][y] = '#'
              changed = true
            end
          when '#'
            if adjacent_seats(x, y) >= 5
              new_grid[x][y] = 'L'
              changed = true
            end
          end
        end
      end
      return if !changed
      @grid = new_grid
    end
  end

  def occupied_seats
    grid.sum { |row| row.count { |seat| seat == '#' } }
  end
end

input = File.read("input.txt")
system = SeatingSystem.new(input)
system.stabilize
puts system.occupied_seats