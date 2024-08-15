require "json"

def read_happiness_values(filename)
  happiness_map = Hash(String, Hash(String, Int32)).new
  File.open(filename) do |file|
    file.each_line do |line|
      parts = line.split
      next if parts.size < 11
      from = parts[0]
      to = parts[10][0..-2]
      change = parts[3].to_i
      change = -change if parts[2] == "lose"
      happiness_map[from] ||= Hash(String, Int32).new
      happiness_map[from][to] = change
    end
  end
  happiness_map
end

def get_guest_list(happiness_map)
  happiness_map.keys
end

def calculate_optimal_arrangement(guests, happiness_map)
  max_happiness = 0
  permute(guests, 0, max_happiness, happiness_map)
end

def permute(arr, i, max_happiness, happiness_map)
  if i == arr.size
    happiness = calculate_happiness(arr, happiness_map)
    max_happiness = [max_happiness, happiness].max
    return max_happiness
  end
  
  (i...arr.size).each do |j|
    arr[i], arr[j] = arr[j], arr[i]
    max_happiness = permute(arr, i + 1, max_happiness, happiness_map)
    arr[i], arr[j] = arr[j], arr[i]  # backtrack
  end
  
  max_happiness
end

def calculate_happiness(arrangement, happiness_map)
  happiness = 0
  n = arrangement.size
  (0...n).each do |i|
    left = (i + n - 1) % n
    right = (i + 1) % n
    happiness += happiness_map[arrangement[i]][arrangement[left]] || 0
    happiness += happiness_map[arrangement[i]][arrangement[right]] || 0
  end
  happiness
end

happiness_map = read_happiness_values("input.txt")
guests = get_guest_list(happiness_map)
max_happiness = calculate_optimal_arrangement(guests, happiness_map)
puts max_happiness