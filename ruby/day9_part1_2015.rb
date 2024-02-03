
locations = []
distances = {}

File.open("input.txt").each do |line|
  parts = line.chomp.split(" ")
  locations << parts[0]
  locations << parts[2]
  distances[[parts[0], parts[2]]] = parts[4].to_i
  distances[[parts[2], parts[0]]] = parts[4].to_i
end

locations.uniq!

min_distance = Float::INFINITY

locations.permutation.each do |perm|
  distance = 0
  perm.each_cons(2) do |pair|
    distance += distances[pair]
  end
  min_distance = [min_distance, distance].min
end

puts min_distance
