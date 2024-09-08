ROCKY, WET, NARROW = 0, 1, 2

def geologic_index(x, y, target_x, target_y, memo)
  return 0 if (x == 0 && y == 0) || (x == target_x && y == target_y)
  return x * 16807 if y == 0
  return y * 48271 if x == 0
  memo[[x-1, y]] * memo[[x, y-1]]
end

def erosion_level(geo_index, depth)
  (geo_index + depth) % 20183
end

def region_type(erosion)
  erosion % 3
end

def calculate_risk(depth, target_x, target_y)
  memo = {}
  risk = 0

  (0..target_y).each do |y|
    (0..target_x).each do |x|
      geo_index = geologic_index(x, y, target_x, target_y, memo)
      erosion = erosion_level(geo_index, depth)
      memo[[x, y]] = erosion
      risk += region_type(erosion)
    end
  end

  risk
end

# Read input
depth, target = File.read('input.txt').split("\n")
depth = depth.split(': ')[1].to_i
target_x, target_y = target.split(': ')[1].split(',').map(&:to_i)

# Calculate and print result
puts calculate_risk(depth, target_x, target_y)
