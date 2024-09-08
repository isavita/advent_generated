require 'set'

def simulate_sand(rocks, part2 = false)
  sand = Set.new
  max_y = rocks.map { |x, y| y }.max
  floor_y = max_y + 2

  loop do
    x, y = 500, 0
    break if sand.include?([x, y])  # Source is blocked (Part 2)

    loop do
      if part2 && y + 1 == floor_y
        sand.add([x, y])
        break
      elsif !part2 && y > max_y
        return sand.size  # Sand falls into abyss (Part 1)
      elsif !rocks.include?([x, y + 1]) && !sand.include?([x, y + 1])
        y += 1
      elsif !rocks.include?([x - 1, y + 1]) && !sand.include?([x - 1, y + 1])
        x -= 1
        y += 1
      elsif !rocks.include?([x + 1, y + 1]) && !sand.include?([x + 1, y + 1])
        x += 1
        y += 1
      else
        sand.add([x, y])
        break
      end
    end
  end

  sand.size
end

rocks = Set.new
File.readlines('input.txt', chomp: true).each do |line|
  points = line.split(' -> ').map { |p| p.split(',').map(&:to_i) }
  points.each_cons(2) do |(x1, y1), (x2, y2)|
    ([x1, x2].min..[x1, x2].max).each do |x|
      ([y1, y2].min..[y1, y2].max).each do |y|
        rocks.add([x, y])
      end
    end
  end
end

puts "Part 1: #{simulate_sand(rocks)}"
puts "Part 2: #{simulate_sand(rocks, true)}"
