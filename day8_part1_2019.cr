
data = File.read("input.txt").chomp

width = 25
height = 6
layer_size = width * height

min_zeros = layer_size + 1
result = 0

(0...data.size).step(layer_size) do |i|
  layer = data[i, layer_size]
  zero_count = 0
  one_count = 0
  two_count = 0

  layer.each_char do |pixel|
    case pixel
    when '0'
      zero_count += 1
    when '1'
      one_count += 1
    when '2'
      two_count += 1
    end
  end

  if zero_count < min_zeros
    min_zeros = zero_count
    result = one_count * two_count
  end
end

puts result
