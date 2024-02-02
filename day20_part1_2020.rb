
input = File.read('input.txt').split("\n\n")

tiles = input.map do |tile|
  id, *lines = tile.split("\n")
  [id.scan(/\d+/).first.to_i, lines.map(&:chars)]
end

edges = Hash.new { |h, k| h[k] = [] }

tiles.each do |id, tile|
  edges[id] += [
    tile.first,
    tile.last,
    tile.map(&:first),
    tile.map(&:last),
    tile.first.reverse,
    tile.last.reverse,
    tile.map(&:first).reverse,
    tile.map(&:last).reverse
  ].map(&:join)
end

corner_tiles = tiles.select do |id, tile|
  edges[id].count { |edge| edges.values.flatten.count(edge) == 1 } == 4
end

puts corner_tiles.map { |id, _| id }.reduce(:*)
