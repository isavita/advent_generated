#!/usr/bin/env ruby
class MinHeap
  def initialize; @nodes = []; end
  def push(item)
    @nodes << item
    i = @nodes.size - 1
    while i > 0
      p = (i - 1) / 2
      break if @nodes[p].energy_used <= @nodes[i].energy_used
      @nodes[p], @nodes[i] = @nodes[i], @nodes[p]
      i = p
    end
  end
  def pop
    return nil if @nodes.empty?
    min = @nodes[0]
    if @nodes.size == 1
      @nodes.pop
      return min
    end
    @nodes[0] = @nodes.pop
    i = 0
    n = @nodes.size
    loop do
      l = 2*i+1; r = 2*i+2; smallest = i
      smallest = l if l < n && @nodes[l].energy_used < @nodes[smallest].energy_used
      smallest = r if r < n && @nodes[r].energy_used < @nodes[smallest].energy_used
      break if smallest == i
      @nodes[i], @nodes[smallest] = @nodes[smallest], @nodes[i]
      i = smallest
    end
    min
  end
  def empty?; @nodes.empty?; end
end

ROOM_COORD_TO_WANT = {
  [2,3]=>"A", [3,3]=>"A",
  [2,5]=>"B", [3,5]=>"B",
  [2,7]=>"C", [3,7]=>"C",
  [2,9]=>"D", [3,9]=>"D"
}
FRONT_OF_ROOMS = { [1,3]=>true, [1,5]=>true, [1,7]=>true, [1,9]=>true }

class State
  attr_accessor :grid, :energy_used, :path
  def initialize(grid, energy_used=0, path="")
    @grid = grid
    @energy_used = energy_used
    @path = path
  end
  def copy
    new_grid = @grid.map(&:dup)
    State.new(new_grid, @energy_used, @path)
  end
  def done?(room)
    room.each { |coord, want| return false if @grid[coord[0]][coord[1]] != want }
    true
  end
  def key
    @grid.map(&:join).join("\n")
  end
  def get_unsettled_coords(room)
    unsettled = []
    (1...@grid[0].size).each { |c| unsettled << [1,c] if "ABCD".include?(@grid[1][c]) }
    [3,5,7,9].each do |c|
      room_full = true
      ((@grid.size-2).downto(2)).each do |r|
        coord = [r,c]
        want = room[coord]
        got = @grid[r][c]
        if got != "."
          if got != want
            room_full = false
            unsettled << coord
          elsif !room_full
            unsettled << coord
          end
        end
      end
    end
    unsettled
  end
  def get_next_possible_moves(from, room)
    ch = @grid[from[0]][from[1]]
    raise "invalid #{ch}" unless "ABCD".include?(ch)
    possibilities = []
    start_hall = (from[0] == 1)
    queue = [from]
    seen = {from => true}
    until queue.empty?
      cur = queue.shift
      if cur != from
        unless FRONT_OF_ROOMS[cur]
          if !room.has_key?(cur)
            possibilities << cur unless start_hall
          else
            want = room[cur]
            if want == ch
              stuck = false
              deeper = false
              ((cur[0]+1)...(@grid.size-1)).each do |r|
                c = @grid[r][cur[1]]
                deeper = true if c == "."
                if c != "." && c != ch
                  stuck = true
                  break
                end
              end
              possibilities << cur if !deeper && !stuck
            end
          end
        end
      end
      [[-1,0],[1,0],[0,-1],[0,1]].each do |d|
        nr, nc = cur[0]+d[0], cur[1]+d[1]
        nxt = [nr,nc]
        next if seen[nxt]
        if @grid[nr][nc]=="."
          queue << nxt
          seen[nxt] = true
        end
      end
    end
    possibilities
  end
end

def calc_energy(ch, start, fin)
  dist = (fin[1]-start[1]).abs + (start[0]-1) + (fin[0]-1)
  { "A"=>1, "B"=>10, "C"=>100, "D"=>1000 }[ch] * dist
end

def parse_input(input)
  grid = input.lines.map { |line| line.chomp.chars }
  State.new(grid)
end

def amphipod(input)
  start = parse_input(input)
  heap = MinHeap.new
  heap.push(start)
  seen = {}
  until heap.empty?
    cur = heap.pop
    k = cur.key
    next if seen[k]
    seen[k] = true
    return cur.energy_used if cur.done?(ROOM_COORD_TO_WANT)
    cur.get_unsettled_coords(ROOM_COORD_TO_WANT).each do |from|
      cur.get_next_possible_moves(from, ROOM_COORD_TO_WANT).each do |to|
        raise "invalid move" if cur.grid[to[0]][to[1]] != "."
        cp = cur.copy
        cp.energy_used += calc_energy(cp.grid[from[0]][from[1]], from, to)
        cp.path << "#{cp.grid[from[0]][from[1]]}#{from}->#{to}{#{cp.energy_used}},"
        cp.grid[to[0]][to[1]], cp.grid[from[0]][from[1]] = cp.grid[from[0]][from[1]], "."
        heap.push(cp)
      end
    end
  end
  raise "no solution found"
end

input = File.read("input.txt").strip
puts amphipod(input)