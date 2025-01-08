
def solve
  happiness_map = read_happiness_values("input.txt")
  add_yourself(happiness_map)
  guests = happiness_map.keys.to_a
  max_happiness = calculate_optimal_arrangement(guests, happiness_map)
  puts max_happiness
end

def read_happiness_values(filename)
  happiness_map = Hash(String, Hash(String, Int32)).new
  File.each_line(filename) do |line|
    parts = line.split
    next if parts.size < 11
    from = parts[0]
    to = parts[10][0..-2]
    change = parts[3].to_i
    change = -change if parts[2] == "lose"
    happiness_map[from] ||= Hash(String, Int32).new
    happiness_map[from][to] = change
  end
  happiness_map
end

def add_yourself(happiness_map : Hash(String, Hash(String, Int32)))
  happiness_map["You"] = Hash(String, Int32).new
  happiness_map.keys.each do |guest|
    happiness_map[guest]["You"] = 0
    happiness_map["You"][guest] = 0
  end
end

def calculate_optimal_arrangement(guests : Array(String), happiness_map : Hash(String, Hash(String, Int32)))
  max_happiness = 0
  guests.permutations.each do |arrangement|
    happiness = calculate_happiness(arrangement, happiness_map)
    max_happiness = happiness if happiness > max_happiness
  end
  max_happiness
end

def calculate_happiness(arrangement : Array(String), happiness_map : Hash(String, Hash(String, Int32)))
  happiness = 0
  n = arrangement.size
  (0...n).each do |i|
    left = (i + n - 1) % n
    right = (i + 1) % n
    happiness += happiness_map[arrangement[i]][arrangement[left]]
    happiness += happiness_map[arrangement[i]][arrangement[right]]
  end
  happiness
end

solve
