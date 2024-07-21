
file = File.read("input.txt").strip
width, height = 25, 6
layer_size = width * height
final_image = Array.new(layer_size, '2')

file.chars.each_slice(layer_size) do |layer|
  layer.each_with_index do |pixel, j|
    final_image[j] = pixel if final_image[j] == '2'
  end
end

puts "Decoded image:"
final_image.each_slice(width) do |row|
  puts row.map { |pixel| pixel == '0' ? ' ' : '#' }.join
end
