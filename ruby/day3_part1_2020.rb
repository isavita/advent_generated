
input = File.readlines('input.txt').map(&:chomp)

trees = 0
x = 0
y = 0

while y < input.length - 1
  x = (x + 3) % input[0].length
  y += 1
  trees += 1 if input[y][x] == '#'
end

puts trees
