
locations = []
distances = {}

File.open("input.txt").each do |line|
  parts = line.chomp.split(" ")
  locations << parts[0]
  locations << parts[2]
  distances[[parts[0], parts[2]].sort] = parts[4].to_i
end

locations.uniq!

all_routes = locations.permutation.to_a

shortest_distance = Float::INFINITY
longest_distance = 0

all_routes.each do |route|
  distance = 0
  route.each_cons(2) do |pair|
    distance += distances[pair.sort]
  end
  shortest_distance = [shortest_distance, distance].min
  longest_distance = [longest_distance, distance].max
end

puts shortest_distance
puts longest_distance
