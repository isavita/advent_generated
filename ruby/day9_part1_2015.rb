
def read_and_parse_input(filename)
  distances = Hash.new { |h, k| h[k] = {} }
  File.foreach(filename) do |line|
    from, _, to, _, dist = line.split
    distance = dist.to_i
    distances[from][to] = distances[to][from] = distance
  end
  distances
end

def find_shortest_route(distances)
  locations = distances.keys
  min_distance = Float::INFINITY
  locations.permutation.each do |route|
    dist = route.each_cons(2).sum { |a, b| distances[a][b] }
    min_distance = dist if dist < min_distance
  end
  min_distance
end

distances = read_and_parse_input('input.txt')
puts find_shortest_route(distances)
