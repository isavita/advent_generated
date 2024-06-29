
def read_and_parse_input(filename)
  distances = Hash.new { |h, k| h[k] = {} }
  File.foreach(filename) do |line|
    from, _, to, _, dist = line.split
    distance = dist.to_i
    distances[from][to] = distances[to][from] = distance
  end
  distances
end

def find_longest_route(distances)
  locations = distances.keys
  max_distance = 0
  locations.permutation.each do |route|
    distance = route.each_cons(2).sum { |a, b| distances[a][b] }
    max_distance = distance if distance > max_distance
  end
  max_distance
end

distances = read_and_parse_input("input.txt")
puts find_longest_route(distances)
