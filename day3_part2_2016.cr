
numbers = File.read("input.txt").lines.map { |line| line.split.map { |num| num.to_i } }

valid_triangles = 0
(0...numbers[0].size).each do |i|
  (0...numbers.size).step(3) do |j|
    if j + 2 < numbers.size && is_valid_triangle(numbers[j][i], numbers[j + 1][i], numbers[j + 2][i])
      valid_triangles += 1
    end
  end
end

puts valid_triangles

def is_valid_triangle(a, b, c)
  a + b > c && a + c > b && b + c > a
end
