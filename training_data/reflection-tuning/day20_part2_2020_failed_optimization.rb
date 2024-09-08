class Tile
  attr_reader :id, :data

  def initialize(id, data)
    @id = id
    @data = data
    @edges = compute_edges
  end

  def edges
    @edges.values
  end

  def rotate
    @data = @data.map(&:chars).transpose.map(&:reverse).map(&:join)
    @edges = compute_edges
  end

  def flip
    @data = @data.map(&:reverse)
    @edges = compute_edges
  end

  private

  def compute_edges
    {
      top: @data.first,
      bottom: @data.last,
      left: @data.map { |row| row[0] }.join,
      right: @data.map { |row| row[-1] }.join
    }
  end
end

def parse_input(input)
  input.split("\n\n").map do |tile_data|
    lines = tile_data.split("\n")
    id = lines.shift.scan(/\d+/).first.to_i
    Tile.new(id, lines)
  end
end

def find_corners(tiles)
  edge_count = Hash.new(0)
  tiles.each do |tile|
    tile.edges.each { |edge| edge_count[edge] += 1 }
  end

  tiles.select do |tile|
    tile.edges.count { |edge| edge_count[edge] == 1 } == 2
  end
end

def assemble_image(tiles)
  size = Math.sqrt(tiles.size).to_i
  grid = Array.new(size) { Array.new(size) }
  used_tiles = Set.new

  # Place first corner tile
  corner = find_corners(tiles).first
  grid[0][0] = corner
  used_tiles.add(corner)

  # Fill the grid
  (0...size).each do |row|
    (0...size).each do |col|
      next if row == 0 && col == 0
      grid[row][col] = find_matching_tile(grid, row, col, tiles, used_tiles)
    end
  end

  grid
end

def find_matching_tile(grid, row, col, tiles, used_tiles)
  tiles.each do |tile|
    next if used_tiles.include?(tile)
    4.times do
      if matches_neighbors?(grid, row, col, tile)
        used_tiles.add(tile)
        return tile
      end
      tile.rotate
    end
    tile.flip
    4.times do
      if matches_neighbors?(grid, row, col, tile)
        used_tiles.add(tile)
        return tile
      end
      tile.rotate
    end
  end
  nil
end

def matches_neighbors?(grid, row, col, tile)
  size = grid.size
  return false if row > 0 && grid[row-1][col].edges[2] != tile.edges[0].reverse
  return false if col > 0 && grid[row][col-1].edges[1] != tile.edges[3]
  true
end

def remove_borders(image)
  image.map do |row|
    row.map do |tile|
      tile.data[1..-2].map { |line| line[1..-2] }
    end
  end
end

def find_sea_monsters(image)
  monster = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  ]
  monster_coords = monster.each_with_index.flat_map do |row, y|
    row.chars.each_with_index.select { |c, _| c == '#' }.map { |_, x| [y, x] }
  end

  count = 0
  (0..image.size-monster.size).each do |y|
    (0..image.first.size-monster.first.size).each do |x|
      if monster_coords.all? { |dy, dx| image[y+dy][x+dx] == '#' }
        count += 1
        monster_coords.each { |dy, dx| image[y+dy][x+dx] = 'O' }
      end
    end
  end
  count
end

input = File.read('input.txt')
tiles = parse_input(input)

# Part 1
corners = find_corners(tiles)
puts "Part 1: #{corners.map(&:id).inject(:*)}"

# Part 2
assembled = assemble_image(tiles)
image = remove_borders(assembled).flat_map { |row| row.transpose.map(&:join) }

8.times do |i|
  image = image.map(&:chars).transpose.map(&:reverse).map(&:join) if i == 4
  image = image.map(&:reverse) if i % 4 == 0
  monsters = find_sea_monsters(image)
  if monsters > 0
    roughness = image.join.count('#')
    puts "Part 2: #{roughness}"
    break
  end
end
