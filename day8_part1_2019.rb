
input = File.read('input.txt').chomp
width = 25
height = 6
layers = input.scan(/.{#{width * height}}/)

fewest_zeros_layer = layers.min_by { |layer| layer.count('0') }
result = fewest_zeros_layer.count('1') * fewest_zeros_layer.count('2')

puts result
