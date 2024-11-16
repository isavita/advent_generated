
class LobbyLayout
  DIRECTIONS = {
    "e"  => {x: 2, y: 0},
    "se" => {x: 1, y: -1},
    "sw" => {x: -1, y: -1},
    "w"  => {x: -2, y: 0},
    "nw" => {x: -1, y: 1},
    "ne" => {x: 1, y: 1}
  }

  def initialize
    @black_tiles = Set(Tuple(Int32, Int32)).new
  end

  def parse_input(input)
    input.each_line do |line|
      flip_tile(parse_path(line))
    end
  end

  def parse_path(path)
    x, y = 0, 0
    i = 0
    while i < path.size
      if path[i] == 'n' || path[i] == 's'
        dir = path[i, 2]
        i += 2
      else
        dir = path[i].to_s
        i += 1
      end
      move = DIRECTIONS[dir]
      x += move[:x]
      y += move[:y]
    end
    {x, y}
  end

  def flip_tile(coord)
    if @black_tiles.includes?(coord)
      @black_tiles.delete(coord)
    else
      @black_tiles.add(coord)
    end
  end

  def count_black_tiles
    @black_tiles.size
  end

  def simulate_days(days)
    days.times do
      next_black_tiles = Set(Tuple(Int32, Int32)).new
      candidates = @black_tiles.flat_map { |tile| adjacent_tiles(tile) }.to_set

      candidates.each do |tile|
        black_neighbors = count_black_neighbors(tile)
        if @black_tiles.includes?(tile)
          next_black_tiles.add(tile) if black_neighbors == 1 || black_neighbors == 2
        else
          next_black_tiles.add(tile) if black_neighbors == 2
        end
      end

      @black_tiles = next_black_tiles
    end
    @black_tiles.size
  end

  private def adjacent_tiles(tile)
    x, y = tile
    DIRECTIONS.values.map { |move| {x + move[:x], y + move[:y]} }
  end

  private def count_black_neighbors(tile)
    adjacent_tiles(tile).count { |adj_tile| @black_tiles.includes?(adj_tile) }
  end
end

# Read input from file
input = File.read("input.txt")

# Part 1: Count initial black tiles
lobby = LobbyLayout.new
lobby.parse_input(input)
puts "Part 1: #{lobby.count_black_tiles}"

# Part 2: Simulate 100 days
puts "Part 2: #{lobby.simulate_days(100)}"
