
# frozen_string_literal: true

DIR_TOP    = {0, -1}
DIR_RIGHT  = {1, 0}
DIR_BOTTOM = {0, 1}
DIR_LEFT   = {-1, 0}
DIRS = [DIR_TOP, DIR_RIGHT, DIR_BOTTOM, DIR_LEFT]

EMPTY = '.'
START = 'S'
VERT   = '|'
HORZ   = '-'
TL     = 'J'
TR     = 'L'
BL     = '7'
BR     = 'F'

VERT_PIPE   = Set{DIR_TOP, DIR_BOTTOM}
HORZ_PIPE   = Set{DIR_LEFT, DIR_RIGHT}
TL_PIPE     = Set{DIR_TOP, DIR_LEFT}
TR_PIPE     = Set{DIR_TOP, DIR_RIGHT}
BL_PIPE     = Set{DIR_BOTTOM, DIR_LEFT}
BR_PIPE     = Set{DIR_BOTTOM, DIR_RIGHT}

TILE_TO_PIPE = {
  VERT => VERT_PIPE,
  HORZ => HORZ_PIPE,
  TL   => TL_PIPE,
  TR   => TR_PIPE,
  BL   => BL_PIPE,
  BR   => BR_PIPE
}

def build_grid(lines : Array(String)) : Hash({Int32, Int32}, Char)
  grid = {} of {Int32, Int32} => Char
  lines.each_with_index do |line, y|
    line.each_char.with_index do |ch, x|
      grid[{x, y}] = ch unless ch == EMPTY
    end
  end
  grid
end

def find_start(grid) : {Int32, Int32}
  grid.each do |coord, ch|
    return coord if ch == START
  end
  {0, 0}
end

def pipe_from_tile(ch : Char) : Set({Int32, Int32})
  TILE_TO_PIPE[ch]? || Set({Int32, Int32}).new
end

def pipe_from_neighbors(coord : {Int32, Int32}, grid) : Set({Int32, Int32})
  result = Set({Int32, Int32}).new
  DIRS.each do |dir|
    n = {coord[0] + dir[0], coord[1] + dir[1]}
    if (ch = grid[n]?) && (pipe = pipe_from_tile(ch)).includes?({-dir[0], -dir[1]})
      result << dir
    end
  end
  result
end

def path_finding(start : {Int32, Int32}, grid) : Array({Int32, Int32})
  path = [start]
  start_pipe = pipe_from_neighbors(start, grid)

  previous_dir = start_pipe.first
  current = {start[0] + previous_dir[0], start[1] + previous_dir[1]}

  while current != start
    path << current
    pipe = pipe_from_tile(grid[current])
    pipe.each do |dir|
      next if dir == {-previous_dir[0], -previous_dir[1]}
      previous_dir = dir
      current = {current[0] + dir[0], current[1] + dir[1]}
      break
    end
  end
  path
end

def solve(lines : Array(String)) : Int32
  grid = build_grid(lines)
  start = find_start(grid)
  path = path_finding(start, grid)
  (path.size // 2)
end

def main
  lines = File.read_lines("input.txt").map(&.strip)
  puts solve(lines)
end

main
