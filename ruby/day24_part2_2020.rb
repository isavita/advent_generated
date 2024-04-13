class Coordinate
  attr_accessor :q, :r

  def initialize(q, r)
    @q = q
    @r = r
  end

  def ==(other)
    @q == other.q && @r == other.r
  end

  def eql?(other)
    self == other
  end

  def hash
    [@q, @r].hash
  end

  def +(other)
    Coordinate.new(@q + other.q, @r + other.r)
  end
end

DIRECTIONS = {
  'e'  => Coordinate.new(1, 0),
  'se' => Coordinate.new(0, 1),
  'sw' => Coordinate.new(-1, 1),
  'w'  => Coordinate.new(-1, 0),
  'nw' => Coordinate.new(0, -1),
  'ne' => Coordinate.new(1, -1)
}

def get_neighbors(tile)
  DIRECTIONS.values.map { |dir| tile + dir }
end

black_tiles = {}

File.open("input.txt") do |file|
  file.each do |line|
    coord = Coordinate.new(0, 0)
    i = 0
    while i < line.strip.length
      case line[i]
      when 'e', 'w'
        dir = line[i]
        i += 1
      when 'n', 's'
        dir = line[i, 2]
        i += 2
      end
      move = DIRECTIONS[dir]
      coord += move
    end
    black_tiles[coord] = !black_tiles[coord]
  end
end

100.times do
  tiles_to_check = {}
  black_tiles.each do |tile, is_black|
    if is_black
      tiles_to_check[tile] = true
      get_neighbors(tile).each { |neighbor| tiles_to_check[neighbor] = true }
    end
  end

  new_black_tiles = {}
  tiles_to_check.each_key do |tile|
    black_neighbor_count = get_neighbors(tile).count { |neighbor| black_tiles[neighbor] }
    if black_tiles[tile] && (black_neighbor_count == 1 || black_neighbor_count == 2)
      new_black_tiles[tile] = true
    elsif !black_tiles[tile] && black_neighbor_count == 2
      new_black_tiles[tile] = true
    end
  end

  black_tiles = new_black_tiles
end

puts black_tiles.size