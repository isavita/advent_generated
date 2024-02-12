
data = File.read("input.txt").strip.split("\n")
depth = data[0].split(" ")[1].to_i
coords = data[1].split(" ")[1].split(",")
target = [coords[0].to_i, coords[1].to_i]

def make_cave_system(depth, target)
  cave = Array(Array(Int32)).new(target[1] + 1) { Array(Int32).new(target[0] + 1, 0) }
  (0..target[1]).each do |y|
    (0..target[0]).each do |x|
      geologic_index = if (x == 0 && y == 0) || (x == target[0] && y == target[1])
                         0
                       elsif y == 0
                         x * 16807
                       elsif x == 0
                         y * 48271
                       else
                         cave[y][x - 1] * cave[y - 1][x]
                       end
      cave[y][x] = (geologic_index + depth) % 20183
    end
  end
  cave
end

def calculate_risk_level(cave, target)
  risk_level = 0
  (0..target[1]).each do |y|
    (0..target[0]).each do |x|
      risk_level += cave[y][x] % 3
    end
  end
  risk_level
end

cave = make_cave_system(depth, target)
risk_level = calculate_risk_level(cave, target)
puts risk_level
