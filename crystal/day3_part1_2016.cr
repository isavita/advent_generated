
File.open("input.txt") do |file|
  valid_triangles = 0
  file.each_line do |line|
    sides = line.split.map { |side| side.to_i }
    next if sides.size != 3

    a, b, c = sides

    if is_valid_triangle(a, b, c)
      valid_triangles += 1
    end
  end

  puts valid_triangles
end

def is_valid_triangle(a, b, c)
  a + b > c && a + c > b && b + c > a
end
