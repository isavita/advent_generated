ITERATIONS = 50
EXPAND_BY = 1

def read_input(filename)
  algorithm = nil
  image = []
  File.readlines(filename).each_with_index do |line, i|
    if i == 0
      algorithm = line.chomp
    elsif !line.strip.empty?
      image << line.chomp.chars.map { |c| c == '#' }
    end
  end
  [algorithm, image]
end

def enhance_image(algorithm, image, use_infinite_lit)
  new_image = Array.new(image.length + EXPAND_BY * 2) { Array.new(image[0].length + EXPAND_BY * 2, false) }
  (-(EXPAND_BY)...image.length + EXPAND_BY).each do |y|
    (-(EXPAND_BY)...image[0].length + EXPAND_BY).each do |x|
      index = 0
      (-1..1).each do |dy|
        (-1..1).each do |dx|
          index <<= 1
          ny, nx = y + dy, x + dx
          if ny >= 0 && ny < image.length && nx >= 0 && nx < image[0].length
            index |= 1 if image[ny][nx]
          elsif use_infinite_lit
            index |= 1
          end
        end
      end
      new_image[y + EXPAND_BY][x + EXPAND_BY] = algorithm[index] == '#'
    end
  end
  new_image
end

def count_lit_pixels(image)
  image.sum { |row| row.count { |pixel| pixel } }
end

algorithm, image = read_input('input.txt')
ITERATIONS.times do |i|
  image = enhance_image(algorithm, image, i % 2 == 1 && algorithm[0] == '#')
end
puts count_lit_pixels(image)