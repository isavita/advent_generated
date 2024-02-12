
forest = File.read("input.txt").lines

def count_trees(forest : Array(String), right : Int32, down : Int32) : Int32
  trees = 0
  x = 0
  width = forest[0].size

  (0...forest.size).step(down) do |y|
    trees += 1 if forest[y][x % width] == '#'
    x += right
  end

  trees
end

trees = count_trees(forest, 3, 1)
puts trees
