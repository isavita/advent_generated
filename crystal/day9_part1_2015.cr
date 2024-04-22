require "file_utils"

def read_and_parse_input(filename)
  distances = {} of String => Hash(String, Int32)
  File.each_line(filename) do |line|
    parts = line.split(" ")
    next if parts.size != 5
    from, to, dist = parts[0], parts[2], parts[4].to_i
    distances[from] ||= {} of String => Int32
    distances[from][to] = dist
    distances[to] ||= {} of String => Int32
    distances[to][from] = dist
  end
  distances
end

def get_unique_locations(distances)
  locations = Set(String).new
  distances.each_key { |from| locations.add(from) }
  distances.each_value { |hash| hash.each_key { |to| locations.add(to) } }
  locations.to_a
end

def find_shortest_route(locations, distances)
  min_distance = -1
  permute(locations, 0, pointerof(min_distance), distances)
  min_distance
end

def permute(arr, i, min_distance, distances)
  if i > arr.size
    return
  end
  if i == arr.size
    dist = calculate_route_distance(arr, distances)
    min_distance.value = dist if min_distance.value == -1 || dist < min_distance.value
    return
  end
  (i...arr.size).each do |j|
    arr[i], arr[j] = arr[j], arr[i]
    permute(arr, i + 1, min_distance, distances)
    arr[i], arr[j] = arr[j], arr[i]
  end
end

def calculate_route_distance(route, distances)
  sum = 0
  (0...route.size - 1).each do |i|
    sum += distances[route[i]][route[i + 1]]
  end
  sum
end

distances = read_and_parse_input("input.txt")
locations = get_unique_locations(distances)
min_distance = find_shortest_route(locations, distances)
puts min_distance