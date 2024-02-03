
def calculate_power_level(x, y, serial)
  rack_id = x + 10
  power_level = rack_id * y
  power_level += serial
  power_level *= rack_id
  power_level = power_level.to_s[-3].to_i
  power_level -= 5
  return power_level
end

def calculate_total_power(x, y, serial)
  total_power = 0
  (0..2).each do |i|
    (0..2).each do |j|
      total_power += calculate_power_level(x + i, y + j, serial)
    end
  end
  return total_power
end

def find_largest_total_power(serial)
  max_power = 0
  top_left = [0, 0]

  (1..298).each do |x|
    (1..298).each do |y|
      power = calculate_total_power(x, y, serial)
      if power > max_power
        max_power = power
        top_left = [x, y]
      end
    end
  end

  return top_left
end

serial = File.read('input.txt').to_i
result = find_largest_total_power(serial)
puts result.join(',')
