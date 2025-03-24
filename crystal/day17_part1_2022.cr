
def read_input(filename : String) : String
  File.read(filename).strip
end

def get_rock_shapes : Array(Array(Tuple(Int32, Int32)))
  [
    [{0, 0}, {1, 0}, {2, 0}, {3, 0}],
    [{1, 0}, {0, 1}, {1, 1}, {2, 1}, {1, 2}],
    [{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}],
    [{0, 0}, {0, 1}, {0, 2}, {0, 3}],
    [{0, 0}, {1, 0}, {0, 1}, {1, 1}],
  ]
end

def can_move(rock : Array(Tuple(Int32, Int32)), direction : Char, chamber : Set(Tuple(Int32, Int32))) : Array(Tuple(Int32, Int32))?
  moved_rock = [] of Tuple(Int32, Int32)
  rock.each do |x, y|
    new_x, new_y = case direction
                   when 'l' then {x - 1, y}
                   when 'r' then {x + 1, y}
                   when 'd' then {x, y - 1}
                   else          raise "Invalid direction"
                   end
    return nil if new_x < 0 || new_x > 6 || new_y < 1 || chamber.includes?({new_x, new_y})
    moved_rock << {new_x, new_y}
  end
  moved_rock
end

def simulate(jet_pattern : String, total_rocks : Int32) : Int32
  rock_shapes = get_rock_shapes
  chamber = Set(Tuple(Int32, Int32)).new
  (0..6).each { |x| chamber << {x, 0} }
  highest_y = 0
  jet_len = jet_pattern.size
  jet_index = 0

  (0...total_rocks).each do |rock_number|
    shape = rock_shapes[rock_number % rock_shapes.size]
    rock_x = 2
    rock_y = highest_y + 4
    rock = shape.map { |dx, dy| {rock_x + dx, rock_y + dy} }

    loop do
      jet_dir = jet_pattern[jet_index % jet_len]
      jet_index += 1
      moved_rock = can_move(rock, jet_dir == '>' ? 'r' : 'l', chamber)
      rock = moved_rock if moved_rock

      moved_down = can_move(rock, 'd', chamber)
      if moved_down
        rock = moved_down
      else
        rock.each do |pos|
          chamber << pos
          highest_y = pos[1] if pos[1] > highest_y
        end
        break
      end
    end
  end
  highest_y
end

def main
  jet_pattern = read_input("input.txt")
  total_rocks = 2022_i32
  final_height = simulate(jet_pattern, total_rocks)
  puts final_height
end

main
