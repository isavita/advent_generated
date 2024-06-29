class SpaceImage
  def initialize(data, width, height)
    @data = data
    @width = width
    @height = height
    @layers = parse_layers
  end

  def parse_layers
    layer_size = @width * @height
    @data.chars.each_slice(layer_size).map(&:join)
  end

  def find_layer_with_fewest_zeros
    @layers.min_by { |layer| layer.count('0') }
  end

  def checksum
    layer = find_layer_with_fewest_zeros
    layer.count('1') * layer.count('2')
  end

  def decode_image
    pixels = Array.new(@width * @height, '2')
    @layers.each do |layer|
      layer.chars.each_with_index do |pixel, i|
        pixels[i] = pixel if pixels[i] == '2'
      end
    end
    pixels.each_slice(@width).map(&:join)
  end

  def print_image
    image = decode_image
    image.each do |row|
      puts row.tr('01', '█ ')
    end
  end
end

# Read input
input = File.read('input.txt').strip

# Part 1
image = SpaceImage.new(input, 25, 6)
puts "Part 1 Answer: #{image.checksum}"

# Part 2
puts "\nPart 2 Answer:"
image.print_image
