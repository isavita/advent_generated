
def read_input(filename)
  file = File.open(filename)
  algorithm = file.gets.chomp
  file.gets # Skip the empty line

  image = []
  while (line = file.gets)
    image << line.chomp.chars
  end

  file.close
  [algorithm, image]
end

def enhance_image(image, algorithm, times)
  times.times do |i|
    image = apply_algorithm(image, algorithm, i % 2 == 1 && algorithm[0] == '#')
  end
  image
end

def apply_algorithm(image, algorithm, flip)
  enhanced_image = Array.new(image.length + 2) { Array.new(image[0].length + 2) }
  enhanced_image.each_with_index do |row, i|
    row.each_index do |j|
      index = calculate_index(i - 1, j - 1, image, flip)
      enhanced_image[i][j] = algorithm[index]
    end
  end
  enhanced_image
end

def calculate_index(i, j, image, flip)
  index = 0
  (-1..1).each do |di|
    (-1..1).each do |dj|
      index <<= 1
      if i + di >= 0 && i + di < image.length && j + dj >= 0 && j + dj < image[0].length
        index |= 1 if image[i + di][j + dj] == '#'
      elsif flip
        index |= 1
      end
    end
  end
  index
end

def count_lit_pixels(image)
  count = 0
  image.each do |row|
    row.each do |pixel|
      count += 1 if pixel == '#'
    end
  end
  count
end

algorithm, image = read_input("input.txt")
image = enhance_image(image, algorithm, 2)
puts count_lit_pixels(image)
