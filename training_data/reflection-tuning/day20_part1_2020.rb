def parse_tiles(input)
  tiles = {}
  current_id = nil
  current_tile = []

  input.each_line do |line|
    line.strip!
    if line.start_with?("Tile")
      tiles[current_id] = current_tile if current_id
      current_id = line.split[1].to_i
      current_tile = []
    elsif !line.empty?
      current_tile << line
    end
  end
  tiles[current_id] = current_tile if current_id

  tiles
end

def extract_borders(tile)
  top = tile.first
  bottom = tile.last
  left = tile.map { |row| row[0] }.join
  right = tile.map { |row| row[-1] }.join
  [top, right, bottom, left]
end

def solve(input)
  tiles = parse_tiles(input)
  border_count = Hash.new(0)

  tiles.each do |id, tile|
    borders = extract_borders(tile)
    borders.each do |border|
      border_count[border] += 1
      border_count[border.reverse] += 1
    end
  end

  corner_tiles = tiles.select do |id, tile|
    borders = extract_borders(tile)
    borders.count { |border| border_count[border] == 1 } == 2
  end

  corner_tiles.keys.inject(:*)
end

input = File.read('input.txt')
puts solve(input)
