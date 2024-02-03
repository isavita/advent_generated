
input = File.readlines('input.txt').map(&:chomp)

trees = input.map { |line| line.split('').map(&:to_i) }

visible_trees = 0

trees.each_with_index do |row, i|
  row.each_with_index do |height, j|
    visible_trees += 1 if i == 0 || i == trees.length - 1 || j == 0 || j == row.length - 1 ||
                         row[0...j].max < height || row[j+1..-1].max < height ||
                         trees[0...i].map { |r| r[j] }.max < height || trees[i+1..-1].map { |r| r[j] }.max < height
  end
end

puts visible_trees
