def main
  grid = {}
  File.readlines('input.txt').each do |line|
    points = line.strip.split(' -> ').map { |p| p.split(',').map(&:to_i) }
    points.each_cons(2) do |(x1, y1), (x2, y2)|
      if x1 == x2
        (min(y1, y2)..max(y1, y2)).each { |y| grid[[x1, y]] = true }
      else
        (min(x1, x2)..max(x1, x2)).each { |x| grid[[x, y1]] = true }
      end
    end
  end

  puts fill(grid)
end

def fill(grid)
  floor = grid.keys.map(&:last).max + 1
  sands, first_floor_touch = 0, 0
  loop do
    sand = [500, 0]
    settled = false
    loop do
      break if settled
      next_sand = [sand[0], sand[1] + 1]
      if next_sand[1] == floor
        grid[sand] = true
        first_floor_touch = sands if first_floor_touch.zero?
        break
      elsif !grid.key?(next_sand)
        sand = next_sand
      else
        next_sand = [sand[0] - 1, sand[1] + 1]
        if !grid.key?(next_sand)
          sand = next_sand
        else
          next_sand = [sand[0] + 1, sand[1] + 1]
          if !grid.key?(next_sand)
            sand = next_sand
          else
            grid[sand] = true
            settled = true
          end
        end
      end
    end
    sands += 1
    break if grid[[500, 0]]
  end
  first_floor_touch
end

def min(a, b)
  a < b ? a : b
end

def max(a, b)
  a > b ? a : b
end

main