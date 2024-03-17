algorithm, image = read_input("input.txt")
image = enhance_image(image, algorithm, 2)
puts count_lit_pixels(image)

def read_input(filename)
  file = File.open(filename)
  algorithm = file.gets.to_s.chomp
  file.gets # Skip the empty line

  image = [] of Array(Char)
  while line = file.gets
    image << line.chars
  end

  return algorithm, image
end

def enhance_image(image, algorithm, times)
  times.times do |i|
    image = apply_algorithm(image, algorithm, i % 2 == 1 && algorithm[0] == '#')
  end
  return image
end

def apply_algorithm(image, algorithm, flip)
  enhanced_image = [] of Array(Char)
  (image.size + 2).times do |i|
    enhanced_image << Array.new(image[0].size + 2, ' ')
  end

  enhanced_image.each_with_index do |row, i|
    row.each_with_index do |_, j|
      index = calculate_index(i - 1, j - 1, image, flip)
      enhanced_image[i][j] = algorithm[index]
    end
  end

  return enhanced_image
end

def calculate_index(i, j, image, flip)
  index = 0
  (-1..1).each do |di|
    (-1..1).each do |dj|
      index <<= 1
      if i + di >= 0 && i + di < image.size && j + dj >= 0 && j + dj < image[0].size
        index |= 1 if image[i + di][j + dj] == '#'
      elsif flip
        index |= 1
      end
    end
  end
  return index
end

def count_lit_pixels(image)
  count = 0
  image.each do |row|
    row.each do |pixel|
      count += 1 if pixel == '#'
    end
  end
  return count
end