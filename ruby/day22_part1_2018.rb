def parse_input(data)
  lines = data.split("\n")
  depth = lines[0].split(" ")[1].to_i
  coords = lines[1].split(" ")[1].split(",")
  x = coords[0].to_i
  y = coords[1].to_i
  [depth, [x, y]]
end

def make_cave_system(depth, target)
  cave = Array.new(target[1] + 1) { Array.new(target[0] + 1) }
  (0..target[1]).each do |y|
    (0..target[0]).each do |x|
      if [x, y] == [0, 0] || [x, y] == target
        geologic_index = 0
      elsif y.zero?
        geologic_index = x * 16807
      elsif x.zero?
        geologic_index = y * 48271
      else
        geologic_index = cave[y][x - 1] * cave[y - 1][x]
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

File.open("input.txt", "r") do |file|
  data = file.read
  depth, target = parse_input(data)
  cave = make_cave_system(depth, target)
  risk_level = calculate_risk_level(cave, target)
  puts risk_level
end