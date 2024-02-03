
input = File.readlines('input.txt').map(&:chomp)

def count_trees(input, right, down)
  trees = 0
  x = 0
  y = 0
  width = input[0].length

  while y < input.length
    trees += 1 if input[y][x] == '#'
    x = (x + right) % width
    y += down
  end

  trees
end

result = count_trees(input, 1, 1) * count_trees(input, 3, 1) * count_trees(input, 5, 1) * count_trees(input, 7, 1) * count_trees(input, 1, 2)
puts result
