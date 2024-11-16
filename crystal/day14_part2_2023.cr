
class ParabolicReflectorDish
  getter platform : Array(Array(Char))

  def initialize(input : Array(String))
    @platform = input.map(&.chars)
  end

  def tilt_north
    @platform.size.times do |col|
      next_row = 0
      @platform.size.times do |row|
        case @platform[row][col]
        when 'O'
          if row != next_row
            @platform[next_row][col] = 'O'
            @platform[row][col] = '.'
          end
          next_row += 1
        when '#'
          next_row = row + 1
        end
      end
    end
  end

  def tilt_west
    @platform.each do |row|
      next_col = 0
      row.size.times do |col|
        case row[col]
        when 'O'
          if col != next_col
            row[next_col] = 'O'
            row[col] = '.'
          end
          next_col += 1
        when '#'
          next_col = col + 1
        end
      end
    end
  end

  def tilt_south
    @platform.size.times do |col|
      next_row = @platform.size - 1
      (@platform.size - 1).downto(0) do |row|
        case @platform[row][col]
        when 'O'
          if row != next_row
            @platform[next_row][col] = 'O'
            @platform[row][col] = '.'
          end
          next_row -= 1
        when '#'
          next_row = row - 1
        end
      end
    end
  end

  def tilt_east
    @platform.each do |row|
      next_col = row.size - 1
      (row.size - 1).downto(0) do |col|
        case row[col]
        when 'O'
          if col != next_col
            row[next_col] = 'O'
            row[col] = '.'
          end
          next_col -= 1
        when '#'
          next_col = col - 1
        end
      end
    end
  end

  def spin_cycle
    tilt_north
    tilt_west
    tilt_south
    tilt_east
  end

  def calculate_load
    @platform.map_with_index do |row, i|
      row.count('O') * (@platform.size - i)
    end.sum
  end

  def to_s
    @platform.map(&.join).join("\n")
  end
end

def solve_part1(input)
  dish = ParabolicReflectorDish.new(input)
  dish.tilt_north
  dish.calculate_load
end

def solve_part2(input)
  dish = ParabolicReflectorDish.new(input)
  seen = {} of String => Int32
  target = 1_000_000_000

  target.times do |cycle|
    dish_state = dish.to_s
    if seen.has_key?(dish_state)
      cycle_length = cycle - seen[dish_state]
      remaining = (target - cycle) % cycle_length
      remaining.times { dish.spin_cycle }
      break
    end
    seen[dish_state] = cycle
    dish.spin_cycle
  end

  dish.calculate_load
end

input = File.read_lines("input.txt")
puts "Part 1: #{solve_part1(input)}"
puts "Part 2: #{solve_part2(input)}"
