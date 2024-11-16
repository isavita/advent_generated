
def parse_input(filename)
  distances = {} of {String, String} => Int32
  locations = Set(String).new

  File.each_line(filename) do |line|
    parts = line.split(" = ")
    route = parts[0].split(" to ")
    distance = parts[1].to_i

    locations.add(route[0])
    locations.add(route[1])
    distances[{route[0], route[1]}] = distance
    distances[{route[1], route[0]}] = distance
  end

  {distances, locations}
end

def calculate_route_distance(route, distances)
  route.each_cons(2).sum do |pair|
    distances[{pair[0], pair[1]}]
  end
end

def find_route_distances(locations, distances)
  routes = locations.to_a.permutations
  routes.map { |route| calculate_route_distance(route, distances) }
end

def solve(filename)
  distances, locations = parse_input(filename)
  route_distances = find_route_distances(locations, distances)

  puts "Part 1: #{route_distances.min}"
  puts "Part 2: #{route_distances.max}"
end

solve("input.txt")
