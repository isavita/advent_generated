class Tile
  attr_reader :id, :data

  def initialize(id, data)
    @id = id
    @data = data
    @borders = calculate_borders
  end

  def borders
    @borders.values
  end

  def rotate
    @data = @data.map(&:chars).transpose.map(&:reverse).map(&:join)
    @borders = calculate_borders
  end

  def flip
    @data = @data.map(&:reverse)
    @borders = calculate_borders
  end

  def matches?(other_tile, side)
    @borders[side] == other_tile.borders.find { |b| b == @borders[side] || b.reverse == @borders[side] }
  end

  private

  def calculate_borders
    {
      top: @data.first,
      bottom: @data.last,
      left: @data.map { |row| row[0] }.join,
      right: @data.map { |row| row[-1] }.join
    }
  end
end

def parse_input(filename)
  File.read(filename).split("\n\n").map do |tile_data|
    lines = tile_data.split("\n")
    id = lines.shift.scan(/\d+/).first.to_i
    Tile.new(id, lines)
  end
end

def find_corner_tiles(tiles)
  tiles.select do |tile|
    matching_sides = tiles.sum do |other|
      next 0 if tile == other
      tile.borders.count { |border| other.borders.include?(border) || other.borders.include?(border.reverse) }
    end
    matching_sides == 2
  end
end

def assemble_image(tiles)
  size = Math.sqrt(tiles.size).to_i
  grid = Array.new(size) { Array.new(size) }
  used_tiles = Set.new

  def backtrack(grid, tiles, used_tiles, row, col)
    return true if row == grid.size

    next_row, next_col = col == grid.size - 1 ? [row + 1, 0] : [row, col + 1]

    tiles.each do |tile|
      next if used_tiles.include?(tile)

      4.times do
        2.times do
          if (row == 0 || tile.matches?(grid[row-1][col], :bottom)) &&
             (col == 0 || tile.matches?(grid[row][col-1], :right))
            grid[row][col] = tile
            used_tiles.add(tile)
            return true if backtrack(grid, tiles, used_tiles, next_row, next_col)
            used_tiles.delete(tile)
          end
          tile.flip
        end
        tile.rotate
      end
    end

    false
  end

  backtrack(grid, tiles, used_tiles, 0, 0)
  grid
end

def remove_borders(grid)
  grid.flat_map do |row|
    row.map { |tile| tile.data[1..-2].map { |line| line[1..-2] } }
       .transpose
       .map(&:join)
  end
end

SEA_MONSTER = [
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "
]

def count_sea_monsters(image)
  sea_monster_height = SEA_MONSTER.size
  sea_monster_width = SEA_MONSTER.first.size
  sea_monster_coords = SEA_MONSTER.flat_map.with_index do |row, y|
    row.chars.map.with_index { |c, x| [y, x] if c == '#' }.compact
  end

  count = 0
  (0..image.size - sea_monster_height).each do |y|
    (0..image.first.size - sea_monster_width).each do |x|
      count += 1 if sea_monster_coords.all? { |dy, dx| image[y + dy][x + dx] == '#' }
    end
  end
  count
end

def solve(filename)
  tiles = parse_input(filename)
  corner_tiles = find_corner_tiles(tiles)
  part1 = corner_tiles.map(&:id).reduce(:*)

  assembled_image = assemble_image(tiles)
  full_image = remove_borders(assembled_image)

  sea_monsters = 0
  4.times do
    2.times do
      sea_monsters = count_sea_monsters(full_image)
      break if sea_monsters > 0
      full_image = full_image.map(&:reverse)
    end
    break if sea_monsters > 0
    full_image = full_image.map(&:chars).transpose.map(&:reverse).map(&:join)
  end

  total_hashes = full_image.join.count('#')
  sea_monster_hashes = SEA_MONSTER.join.count('#')
  part2 = total_hashes - (sea_monsters * sea_monster_hashes)

  puts "Part 1: #{part1}"
  puts "Part 2: #{part2}"
end

solve('input.txt')
