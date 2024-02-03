input = File.readlines('input.txt').map(&:split)

possible_triangles = 0

input.each_slice(3) do |group|
  group.transpose.each do |triangle_sides|
    triangle_sides.map!(&:to_i)
    triangle_sides.sort!
    possible_triangles += 1 if triangle_sides[0] + triangle_sides[1] > triangle_sides[2]
  end
end

puts possible_triangles