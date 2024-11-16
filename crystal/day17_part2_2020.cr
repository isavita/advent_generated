
struct Coordinate4D
  property x, y, z, w

  def initialize(@x : Int32, @y : Int32, @z : Int32, @w : Int32)
  end

  def_equals_and_hash x, y, z, w
end

active_cubes = {} of Coordinate4D => Bool

File.read_lines("input.txt").each_with_index do |line, y|
  line.each_char_with_index do |char, x|
    active_cubes[Coordinate4D.new(x, y, 0, 0)] = true if char == '#'
  end
end

6.times do
  new_active_cubes = {} of Coordinate4D => Bool
  neighbor_counts = {} of Coordinate4D => Int32

  active_cubes.each_key do |coord|
    (-1..1).each do |dw|
      (-1..1).each do |dz|
        (-1..1).each do |dy|
          (-1..1).each do |dx|
            next if dw == 0 && dz == 0 && dy == 0 && dx == 0
            
            neighbor = Coordinate4D.new(
              coord.x + dx, 
              coord.y + dy, 
              coord.z + dz, 
              coord.w + dw
            )
            neighbor_counts[neighbor] ||= 0
            neighbor_counts[neighbor] += 1
          end
        end
      end
    end
  end

  neighbor_counts.each do |coord, count|
    new_active_cubes[coord] = true if count == 3 || (count == 2 && active_cubes.has_key?(coord))
  end

  active_cubes = new_active_cubes
end

puts active_cubes.size
