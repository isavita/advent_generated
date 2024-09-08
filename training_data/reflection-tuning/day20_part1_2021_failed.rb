def enhance(image, algorithm, default_lit)
  min_x, max_x = image.map { |x, _| x }.minmax
  min_y, max_y = image.map { |_, y| y }.minmax
  
  new_image = Set.new
  
  (min_y-1..max_y+1).each do |y|
    (min_x-1..max_x+1).each do |x|
      index = (-1..1).flat_map do |dy|
        (-1..1).map do |dx|
          if image.include?([x+dx, y+dy]) ^ default_lit
            '1'
          else
            '0'
          end
        end
      end.join.to_i(2)
      
      new_image.add([x, y]) if algorithm[index] == '#'
    end
  end
  
  new_image
end

# Read input
algorithm, _, *image_lines = File.readlines('input.txt', chomp: true)
algorithm = algorithm.chars

# Parse initial image
image = Set.new
image_lines.each_with_index do |line, y|
  line.chars.each_with_index do |char, x|
    image.add([x, y]) if char == '#'
  end
end

# Enhance twice
2.times do |i|
  default_lit = i.odd? && algorithm[0] == '#'
  image = enhance(image, algorithm, default_lit)
end

# Count lit pixels
puts image.size
