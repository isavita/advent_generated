
require "compiler/crystal/syntax"

def get_borders(tile)
  return [] of String if tile.empty?
  borders = [tile[0], tile[-1]]
  left, right = "", ""
  tile.each do |row|
    next if row.empty?
    left  += row[0]
    right += row[-1]
  end
  borders + [left, right]
end

tiles = Hash(Int32, Array(String)).new
id = -1
File.read_lines("input.txt").each do |line|
  next if line.empty?
  if line.starts_with?("Tile ")
    id = line[5..-2].to_i
    tiles[id] = [] of String
  elsif id != -1
    tiles[id] << line
  end
end

borders_map = Hash(String, Array(Int32)).new { |h, k| h[k] = [] of Int32 }
tiles.each do |tile_id, tile_data|
  get_borders(tile_data).each do |border|
    borders_map[border] << tile_id
    borders_map[border.reverse] << tile_id
  end
end

corners = tiles.keys.select do |tile_id|
  get_borders(tiles[tile_id]).count { |b| borders_map[b].size == 1 } == 2
end

puts corners.product(1_i64)
