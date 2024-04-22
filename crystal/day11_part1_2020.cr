class SeatingSystem
  property seats : Array(Array(Char))

  def initialize(file_name : String)
    @seats = File.read_lines(file_name).map(&.chars.to_a)
  end

  def adjacent_seats(x : Int, y : Int) : Int
    count = 0
    (-1..1).each do |dx|
      (-1..1).each do |dy|
        next if dx == 0 && dy == 0
        nx, ny = x + dx, y + dy
        if nx >= 0 && nx < @seats.size && ny >= 0 && ny < @seats[0].size
          count += @seats[nx][ny] == '#' ? 1 : 0
        end
      end
    end
    count
  end

  def apply_rules
    new_seats = @seats.map(&.dup)
    @seats.each_with_index do |row, x|
      row.each_with_index do |seat, y|
        adjacent = adjacent_seats(x, y)
        if seat == 'L' && adjacent == 0
          new_seats[x][y] = '#'
        elsif seat == '#' && adjacent >= 4
          new_seats[x][y] = 'L'
        end
      end
    end
    @seats = new_seats
  end

  def stabilize
    loop do
      new_seats = @seats.map(&.dup)
      @seats.each_with_index do |row, x|
        row.each_with_index do |seat, y|
          adjacent = adjacent_seats(x, y)
          if seat == 'L' && adjacent == 0
            new_seats[x][y] = '#'
          elsif seat == '#' && adjacent >= 4
            new_seats[x][y] = 'L'
          end
        end
      end
      return if new_seats == @seats
      @seats = new_seats
    end
  end

  def occupied_seats
    @seats.sum { |row| row.count { |seat| seat == '#' } }
  end
end

seating_system = SeatingSystem.new("input.txt")
seating_system.stabilize
puts "Occupied seats: #{seating_system.occupied_seats}"