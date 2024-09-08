def parse_input(file)
  cave = {}
  max_y = 0

  File.readlines(file, chomp: true).each do |line|
    points = line.split(' -> ').map { |p| p.split(',').map(&:to_i) }
    points.each_cons(2) do |(x1, y1), (x2, y2)|
      ([x1, x2].min..[x1, x2].max).each do |x|
        ([y1, y2].min..[y1, y2].max).each do |y|
          cave[[x, y]] = '#'
          max_y = [max_y, y].max
        end
      end
    end
  end

  [cave, max_y]
end

def drop_sand(cave, max_y, x, y)
  return false if y > max_y

  [[x, y + 1], [x - 1, y + 1], [x + 1, y + 1]].each do |next_x, next_y|
    return drop_sand(cave, max_y, next_x, next_y) unless cave[[next_x, next_y]]
  end

  cave[[x, y]] = 'o'
  true
end

cave, max_y = parse_input('input.txt')
sand_count = 0

while drop_sand(cave, max_y, 500, 0)
  sand_count += 1
end

puts sand_count
