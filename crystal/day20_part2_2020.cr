
class Tile
  getter contents : Array(Array(String))
  getter id : Int32
  def initialize(@contents, @id)
  end
  def initialize(other : Tile)
    @contents = other.contents.map &.dup
    @id = other.id
  end
end

def solve(input : String) : Int32
  tiles = parse_tiles(input)
  edge_size = Math.sqrt(tiles.size).to_i
  assembled = backtrack(tiles, edge_size)
  processed = assembled.map &.map { |t| Tile.new(t.contents[1...-1].map { |r| r[1...-1] }, t.id) }
  image = [] of Array(String)
  processed.each do |row|
    h = row[0].contents.size
    (0...h).each do |i|
      image << row.flat_map { |t| t.contents[i] }.to_a
    end
  end
  final = all_orientations(image).find { |o| monsters(o).any? } || image
  marked = final.map &.dup
  monsters(final).each do |(r, c)|
    marked[r][c] = "O"
  end
  marked.flatten.count("#")
end

def parse_tiles(input : String) : Array(Tile)
  input.split("\n\n").map do |block|
    lines = block.split('\n')
    id = lines[0][5...-1].to_i
    contents = lines[1..].map &.chars.map &.to_s
    Tile.new(contents, id)
  end
end

def backtrack(tiles : Array(Tile), n : Int32) : Array(Array(Tile))
  grid = Array.new(n) { Array(Tile?).new(n, nil) }
  used = Set(Int32).new
  recurse = uninitialized Proc(Int32, Int32, Bool)
  recurse = ->(row : Int32, col : Int32) do
    return true if row == n
    nr = col == n - 1 ? row + 1 : row
    nc = col == n - 1 ? 0 : col + 1
    return recurse.call(nr, nc) if grid[row][col]
    tiles.each_with_index do |tile, idx|
      next if used.includes?(idx)
      all_orientations(tile.contents).each do |o|
        top_ok = row == 0 || edge(o, :top) == edge(grid[row - 1][col].not_nil!.contents, :bottom)
        left_ok = col == 0 || edge(o, :left) == edge(grid[row][col - 1].not_nil!.contents, :right)
        if top_ok && left_ok
          grid[row][col] = Tile.new(o, tile.id)
          used << idx
          return true if recurse.call(nr, nc)
          used.delete(idx)
          grid[row][col] = nil
        end
      end
    end
    false
  end
  recurse.call(0, 0) ? grid.map &.map &.not_nil! : raise("no solution")
end

def edge(grid : Array(Array(String)), side : Symbol) : String
  case side
  when :top    then grid[0].join
  when :bottom then grid[-1].join
  when :left   then grid.map(&.[0]).join
  when :right  then grid.map(&.[-1]).join
  else              raise "bad side"
  end
end

def all_orientations(grid : Array(Array(String))) : Array(Array(Array(String)))
  res = [grid]
  cur = grid
  3.times do
    cur = rotate(cur)
    res << cur
  end
  4.times do |i|
    res << mirror(res[i])
  end
  res
end

def rotate(grid : Array(Array(String))) : Array(Array(String))
  n = grid.size
  m = grid[0].size
  Array.new(m) do |i|
    Array.new(n) do |j|
      grid[n - 1 - j][i]
    end
  end
end

def mirror(grid : Array(Array(String))) : Array(Array(String))
  grid.map &.reverse
end

MONSTER = ["                  # ",
           "#    ##    ##    ###",
           " #  #  #  #  #  #   "]

def monsters(img : Array(Array(String))) : Array(Tuple(Int32, Int32))
  h = MONSTER.size
  w = MONSTER[0].size
  offsets = [] of Tuple(Int32, Int32)
  h.times do |r|
    w.times do |c|
      offsets << {r, c} if MONSTER[r][c] == '#'
    end
  end
  res = [] of Tuple(Int32, Int32)
  (0..img.size - h).each do |r|
    (0..img[0].size - w).each do |c|
      ok = offsets.all? do |(dr, dc)|
        img[r + dr][c + dc] == "#"
      end
      if ok
        offsets.each do |(dr, dc)|
          res << {r + dr, c + dc}
        end
      end
    end
  end
  res
end

input = File.read("input.txt")
puts solve(input)
