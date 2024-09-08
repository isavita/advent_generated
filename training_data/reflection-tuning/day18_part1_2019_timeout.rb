require 'set'

class Map
  attr_reader :grid, :keys, :start

  def initialize(input)
    @grid = input.map(&:chars)
    @keys = {}
    @start = nil
    @grid.each_with_index do |row, y|
      row.each_with_index do |cell, x|
        if cell == '@'
          @start = [y, x]
        elsif ('a'..'z').include?(cell)
          @keys[cell] = [y, x]
        end
      end
    end
  end
end

def shortest_path(map, start, target, keys)
  queue = [[start, 0]]
  visited = Set.new([start])
  
  while !queue.empty?
    pos, dist = queue.shift
    return dist if pos == target
    
    [[0, 1], [0, -1], [1, 0], [-1, 0]].each do |dy, dx|
      ny, nx = pos[0] + dy, pos[1] + dx
      next if ny < 0 || ny >= map.grid.length || nx < 0 || nx >= map.grid[0].length
      cell = map.grid[ny][nx]
      next if cell == '#'
      next if ('A'..'Z').include?(cell) && !keys.include?(cell.downcase)
      new_pos = [ny, nx]
      next if visited.include?(new_pos)
      
      visited.add(new_pos)
      queue << [new_pos, dist + 1]
    end
  end
  
  nil
end

def collect_keys(map)
  all_keys = (1 << map.keys.size) - 1
  queue = [[map.start, 0, 0]]
  cache = {}
  
  while !queue.empty?
    pos, keys, steps = queue.shift
    return steps if keys == all_keys
    
    map.keys.each do |key, key_pos|
      next if keys & (1 << (key.ord - 'a'.ord)) != 0
      
      cache_key = [pos, key_pos, keys]
      dist = cache[cache_key]
      if !dist
        dist = shortest_path(map, pos, key_pos, keys.to_s(2).rjust(26, '0').chars.map.with_index { |b, i| b == '1' ? (i + 97).chr : nil }.compact)
        cache[cache_key] = dist
      end
      
      next unless dist
      
      new_keys = keys | (1 << (key.ord - 'a'.ord))
      queue << [key_pos, new_keys, steps + dist]
    end
    
    queue.sort_by! { |_, _, s| s }
  end
  
  -1
end

input = File.read('input.txt').strip.split("\n")
map = Map.new(input)
puts collect_keys(map)
