def enhance_image(algorithm, image, steps)
  background = '.'
  
  steps.times do |step|
    new_image = {}
    min_x, max_x = image.keys.map(&:first).minmax
    min_y, max_y = image.keys.map(&:last).minmax

    ((min_y - 1)..(max_y + 1)).each do |y|
      ((min_x - 1)..(max_x + 1)).each do |x|
        binary = (-1..1).flat_map do |dy|
          (-1..1).map do |dx|
            pixel = image[[x + dx, y + dy]] || background
            pixel == '#' ? '1' : '0'
          end
        end.join

        index = binary.to_i(2)
        new_image[[x, y]] = algorithm[index]
      end
    end

    image = new_image
    background = algorithm[background == '#' ? 511 : 0]
  end

  image.values.count('#')
end

algorithm = File.readlines('input.txt').first.strip
image = {}
File.readlines('input.txt')[2..-1].each_with_index do |line, y|
  line.strip.chars.each_with_index do |char, x|
    image[[x, y]] = char if char == '#'
  end
end

puts enhance_image(algorithm, image, 2)
