#!/usr/bin/env ruby

class State
  attr_accessor :grid, :energy_used, :path

  def initialize(grid, energy_used, path)
    @grid = grid
    @energy_used = energy_used
    @path = path
  end

  def <=>(other)
    @energy_used <=> other.energy_used
  end

  def to_s
    result = ""
    @grid.each do |row|
      result += row.join + "\n"
    end
    result += "Energy used: #{@energy_used}, Path: #{@path}"
    result
  end

  def copy
    State.new(
      @grid.map(&:dup),
      @energy_used,
      @path
    )
  end

  def is_all_done?(room_coord_to_want_char)
    room_coord_to_want_char.all? do |coord, want|
      @grid[coord[0]][coord[1]] == want
    end
  end

  def get_unsettled_coords(room_coord_to_want_char)
    unsettled = []
    
    # Check hallway positions
    (1...@grid[0].length).each do |col|
      unsettled << [1, col] if @grid[1][col] =~ /[ABCD]/
    end

    # Check rooms
    [3, 5, 7, 9].each do |col|
      room_full_from_back = true
      (@grid.length - 2).downto(2) do |row|
        coord = [row, col]
        want_char = room_coord_to_want_char[coord]
        got_char = @grid[row][col]
        if got_char != "."
          if got_char != want_char
            room_full_from_back = false
            unsettled << coord
          elsif got_char == want_char && !room_full_from_back
            unsettled << coord
          end
        end
      end
    end
    unsettled
  end

  def get_next_possible_moves(unsettled_coord, room_coord_to_want_char)
    unsettled_char = @grid[unsettled_coord[0]][unsettled_coord[1]]
    raise "Unexpected character to get next moves for: #{unsettled_char}" unless unsettled_char =~ /[ABCD]/

    possible = []
    started_in_hallway = unsettled_coord[0] == 1

    queue = [unsettled_coord]
    seen = Set.new
    until queue.empty?
      front = queue.shift
      next if seen.include?(front)
      seen.add(front)

      if front != unsettled_coord
        unless [3, 5, 7, 9].include?(front[1]) && front[0] == 1
          want_char = room_coord_to_want_char[front]
          if want_char.nil?
            possible << front unless started_in_hallway
          elsif want_char == unsettled_char
            is_stuck_amphipod = false
            room_has_deeper_open_spaces = false
            ((front[0] + 1)...@grid.length - 1).each do |r|
              char = @grid[r][front[1]]
              if char == "."
                room_has_deeper_open_spaces = true
              elsif char != "." && char != unsettled_char
                is_stuck_amphipod = true
                break
              end
            end
            possible << front if !room_has_deeper_open_spaces && !is_stuck_amphipod
          end
        end
      end

      [[-1, 0], [1, 0], [0, -1], [0, 1]].each do |dr, dc|
        next_row = front[0] + dr
        next_col = front[1] + dc
        if next_row.between?(0, @grid.length - 1) && 
           next_col.between?(0, @grid[0].length - 1) && 
           @grid[next_row][next_col] == "."
          queue << [next_row, next_col]
        end
      end
    end

    possible
  end
end

def calc_energy(char, start, end_pos)
  dist = (end_pos[1] - start[1]).abs
  dist += start[0] - 1
  dist += end_pos[0] - 1
  energy_per_type = {"A" => 1, "B" => 10, "C" => 100, "D" => 1000}
  raise "Unexpected character: #{char}" unless energy_per_type.key?(char)
  energy_per_type[char] * dist
end

def amphipod(input_str)
  start = State.new(
    input_str.lines.map(&:chomp).map(&:chars),
    0,
    ""
  )

  room_coord_to_want_char = {
    [2, 3] => "A", [3, 3] => "A", [4, 3] => "A", [5, 3] => "A",
    [2, 5] => "B", [3, 5] => "B", [4, 5] => "B", [5, 5] => "B",
    [2, 7] => "C", [3, 7] => "C", [4, 7] => "C", [5, 7] => "C",
    [2, 9] => "D", [3, 9] => "D", [4, 9] => "D", [5, 9] => "D"
  }

  # Extend grid for part 2
  2.times { start.grid << nil }
  start.grid[6] = start.grid[4]
  start.grid[5] = start.grid[3]
  start.grid[3] = "  #D#C#B#A#  ".chars
  start.grid[4] = "  #D#B#A#C#  ".chars

  min_heap = PriorityQueue.new
  min_heap.push(start)
  seen_grids = Set.new

  until min_heap.empty?
    front = min_heap.pop
    key = front.grid.map(&:join).join("\n")
    next if seen_grids.include?(key)
    seen_grids.add(key)

    return front.energy_used if front.is_all_done?(room_coord_to_want_char)

    unsettled_coords = front.get_unsettled_coords(room_coord_to_want_char)
    unsettled_coords.each do |unsettled_coord|
      next_moves = front.get_next_possible_moves(unsettled_coord, room_coord_to_want_char)
      next_moves.each do |next_coord|
        if front.grid[next_coord[0]][next_coord[1]] != "."
          raise "Should only be moving to walkable spaces, got #{front.grid[next_coord[0]][next_coord[1]]} at #{next_coord}"
        end

        cp = front.copy
        cp.energy_used += calc_energy(cp.grid[unsettled_coord[0]][unsettled_coord[1]], unsettled_coord, next_coord)
        cp.path += "#{cp.grid[unsettled_coord[0]][unsettled_coord[1]]}#{unsettled_coord}->#{next_coord}(#{cp.energy_used}),"
        cp.grid[next_coord[0]][next_coord[1]], cp.grid[unsettled_coord[0]][unsettled_coord[1]] = cp.grid[unsettled_coord[0]][unsettled_coord[1]], "."
        min_heap.push(cp)
      end
    end
  end

  raise "Should return from loop"
end

# Simple priority queue implementation using a binary heap
class PriorityQueue
  def initialize
    @heap = []
  end

  def push(item)
    @heap << item
    bubble_up(@heap.size - 1)
  end

  def pop
    return nil if empty?
    
    if @heap.size == 1
      return @heap.pop
    end
    
    min = @heap[0]
    @heap[0] = @heap.pop
    bubble_down(0)
    min
  end

  def empty?
    @heap.empty?
  end

  private

  def bubble_up(index)
    parent = (index - 1) / 2
    
    while index > 0 && @heap[parent].energy_used > @heap[index].energy_used
      @heap[parent], @heap[index] = @heap[index], @heap[parent]
      index = parent
      parent = (index - 1) / 2
    end
  end

  def bubble_down(index)
    loop do
      min = index
      left = 2 * index + 1
      right = 2 * index + 2

      min = left if left < @heap.size && @heap[left].energy_used < @heap[min].energy_used
      min = right if right < @heap.size && @heap[right].energy_used < @heap[min].energy_used

      break if min == index

      @heap[index], @heap[min] = @heap[min], @heap[index]
      index = min
    end
  end
end

require 'set'

input_str = File.read("input.txt").strip
puts amphipod(input_str)
