
def read_input(filename : String)
  lines = File.read_lines(filename)
  algorithm = lines[0]
  image = lines[2..-1].map { |line| line.chars.map { |c| c == '#' } }
  {algorithm, image}
end

def enhance_image(algorithm : String, image : Array(Array(Bool)), use_infinite_lit : Bool)
  expand_by = 1
  new_height = image.size + expand_by * 2
  new_width = image[0].size + expand_by * 2
  new_image = Array.new(new_height) { Array.new(new_width, false) }

  (0...new_height).each do |y|
    (0...new_width).each do |x|
      index = 0
      (-1..1).each do |dy|
        (-1..1).each do |dx|
          index <<= 1
          ny = y + dy - expand_by
          nx = x + dx - expand_by
          
          if ny >= 0 && ny < image.size && nx >= 0 && nx < image[0].size
            index |= 1 if image[ny][nx]
          elsif use_infinite_lit
            index |= 1
          end
        end
      end
      new_image[y][x] = algorithm[index] == '#'
    end
  end
  new_image
end

def count_lit_pixels(image : Array(Array(Bool)))
  image.sum { |row| row.count { |pixel| pixel } }
end

def main
  iterations = 50
  algorithm, image = read_input("input.txt")
  
  iterations.times do |i|
    image = enhance_image(algorithm, image, i.odd? && algorithm[0] == '#')
  end
  
  puts count_lit_pixels(image)
end

main
