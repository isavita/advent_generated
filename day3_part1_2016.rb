
triangles = File.readlines('input.txt').map { |line| line.split.map(&:to_i) }

valid_triangles = triangles.select { |sides| sides.permutation(3).all? { |a, b, c| a + b > c } }

puts valid_triangles.count
