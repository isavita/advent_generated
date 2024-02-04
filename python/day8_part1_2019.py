
with open("input.txt", "r") as file:
    data = file.read().strip()

width = 25
height = 6
pixels_per_layer = width * height
num_layers = len(data) // pixels_per_layer

min_zeros = float('inf')
result = 0

for i in range(num_layers):
    layer = data[i*pixels_per_layer : (i+1)*pixels_per_layer]
    zeros = layer.count('0')
    if zeros < min_zeros:
        min_zeros = zeros
        result = layer.count('1') * layer.count('2')

print(result)
