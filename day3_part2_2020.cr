
lines = File.read("input.txt").lines

slopes = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2],
]

product = 1
slopes.each do |slope|
  tree_count = 0
  pos = 0
  (0..lines.size - 1).step(slope[1]) do |i|
    tree_count += 1 if lines[i][pos] == '#'
    pos = (pos + slope[0]) % lines[i].size
  end
  product *= tree_count
end

puts product
