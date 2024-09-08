require 'set'

def enhance(algorithm, image, iterations)
  lit_pixels = Set.new(image)
  min_x, max_x = image.map(&:first).minmax
  min_y, max_y = image.map(&:last).minmax
  infinite_lit = false

  iterations.times do |i|
    new_lit_pixels = Set.new
    ((min_y - 1)..(max_y + 1)).each do |y|
      ((min_x - 1)..(max_x + 1)).each do |x|
        index = (-1..1).sum do |dy|
          (-1..1).sum do |dx|
            bit = if (min_x..max_x).cover?(x + dx) && (min_y..max_y).cover?(y + dy)
              lit_pixels.include?([x + dx, y + dy]) ? 1 : 0
            else
              infinite_lit ? 1 : 0
            end
            bit << (8 - (dy * 3 + dx + 4))
          end
        end
        new_lit_pixels.add([x, y]) if algorithm[index] == '#'
      end
    end
    lit_pixels = new_lit_pixels
    min_x -= 1; max_x += 1; min_y -= 1; max_y += 1
    infinite_lit = infinite_lit ? algorithm[-1] == '#' : algorithm[0] == '#'
  end

  lit_pixels.size
end

input = File.read('input.txt').split("\n\n")
algorithm = input[0].strip
image = input[1].split("\n").each_with_index.flat_map do |line, y|
  line.chars.each_with_index.map { |c, x| [x, y] if c == '#' }
end.compact

puts "Part 1: #{enhance(algorithm, image, 2)}"
puts "Part 2: #{enhance(algorithm, image, 50)}"
