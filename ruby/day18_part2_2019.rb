# Simple binary min-heap implementation
class MinHeap
  def initialize
    @heap = []
  end

  def push(value)
    @heap << value
    bubble_up(@heap.size - 1)
  end

  def pop
    return nil if empty?
    
    swap(0, @heap.size - 1)
    min = @heap.pop
    bubble_down(0) unless empty?
    min
  end

  def empty?
    @heap.empty?
  end

  private

  def bubble_up(index)
    parent = (index - 1) / 2
    
    if parent >= 0 && (@heap[parent][0] > @heap[index][0])
      swap(parent, index)
      bubble_up(parent)
    end
  end

  def bubble_down(index)
    min = index
    left = 2 * index + 1
    right = 2 * index + 2

    min = left if left < @heap.size && @heap[left][0] < @heap[min][0]
    min = right if right < @heap.size && @heap[right][0] < @heap[min][0]

    if min != index
      swap(min, index)
      bubble_down(min)
    end
  end

  def swap(i, j)
    @heap[i], @heap[j] = @heap[j], @heap[i]
  end
end

# Read the map from 'input.txt'
original_map = File.readlines('input.txt').map(&:chomp).reject(&:empty?).map(&:chars)

# Modify the map as per Part Two instructions
# Find the position of '@' surrounded by open spaces ('.')
found = false
(1..original_map.length - 2).each do |y|
  (1..original_map[0].length - 2).each do |x|
    if original_map[y][x] == '@'
      # Check if the surrounding positions are open space ('.')
      if original_map[y-1][x] == '.' && original_map[y+1][x] == '.' &&
         original_map[y][x-1] == '.' && original_map[y][x+1] == '.'
        # Modify the map
        original_map[y-1][x-1..x+1] = ['@', '#', '@']
        original_map[y][x-1..x+1] = ['#', '#', '#']
        original_map[y+1][x-1..x+1] = ['@', '#', '@']
        found = true
        break
      end
    end
  end
  break if found
end

unless found
  puts "Error: Could not find the '@' symbol surrounded by open spaces."
  exit(1)
end

# Now, find the positions of the 4 robots
robot_positions = []
original_map.each_with_index do |row, y|
  row.each_with_index do |cell, x|
    robot_positions << [x, y] if cell == '@'
  end
end

# Collect all keys and doors
keys = {}
doors = {}
all_keys = Set.new
original_map.each_with_index do |row, y|
  row.each_with_index do |cell, x|
    if cell.match?(/[a-z]/)
      keys[cell] = [x, y]
      all_keys.add(cell)
    elsif cell.match?(/[A-Z]/)
      doors[cell] = [x, y]
    end
  end
end

# Precompute reachable keys and distances for each robot
def bfs(start_pos, original_map)
  queue = [[start_pos[0], start_pos[1], 0, Set.new]]
  visited = Set.new
  results = {}

  until queue.empty?
    x, y, dist, required_keys = queue.shift
    next if visited.include?([x, y])
    visited.add([x, y])

    cell = original_map[y][x]
    if cell.match?(/[a-z]/) && !required_keys.include?(cell)
      # Found a key
      results[cell] = [dist, required_keys.clone]
      required_keys.add(cell)
    end

    [[-1,0], [1,0], [0,-1], [0,1]].each do |dx, dy|
      nx, ny = x + dx, y + dy
      if ny >= 0 && ny < original_map.length && nx >= 0 && nx < original_map[0].length
        ncell = original_map[ny][nx]
        if ncell != '#'
          if ncell.match?(/[A-Z]/)
            # Door, need the key
            n_required_keys = required_keys.clone
            n_required_keys.add(ncell.downcase)
            queue << [nx, ny, dist+1, n_required_keys]
          else
            queue << [nx, ny, dist+1, required_keys.clone]
          end
        end
      end
    end
  end
  results
end

# For each robot, compute reachable keys
robot_reachable_keys = robot_positions.map { |pos| bfs(pos, original_map) }

# Precompute distances between keys
key_positions = keys.clone
robot_positions.each_with_index do |pos, i|
  key_positions["@#{i}"] = pos
end

def key_bfs(start_key, key_positions, original_map)
  start_pos = key_positions[start_key]
  queue = [[start_pos[0], start_pos[1], 0, Set.new]]
  visited = Set.new
  results = {}

  until queue.empty?
    x, y, dist, required_keys = queue.shift
    next if visited.include?([x, y])
    visited.add([x, y])

    cell = original_map[y][x]
    if cell.match?(/[a-z]/) && cell != start_key.downcase && !required_keys.include?(cell)
      # Found a key
      results[cell] = [dist, required_keys.clone]
      required_keys.add(cell)
    end

    [[-1,0], [1,0], [0,-1], [0,1]].each do |dx, dy|
      nx, ny = x + dx, y + dy
      if ny >= 0 && ny < original_map.length && nx >= 0 && nx < original_map[0].length
        ncell = original_map[ny][nx]
        if ncell != '#'
          if ncell.match?(/[A-Z]/)
            # Door, need the key
            n_required_keys = required_keys.clone
            n_required_keys.add(ncell.downcase)
            queue << [nx, ny, dist+1, required_keys.clone]
          else
            queue << [nx, ny, dist+1, required_keys.clone]
          end
        end
      end
    end
  end
  results
end

# Build a graph of keys and distances
key_graph = {}
all_nodes = keys.keys + robot_positions.length.times.map { |i| "@#{i}" }
all_nodes.each do |key|
  key_graph[key] = key_bfs(key, key_positions, original_map)
end

def bitmask(keys_set)
  mask = 0
  keys_set.each do |k|
    mask |= 1 << (k.ord - 'a'.ord)
  end
  mask
end

def dijkstra(robot_positions, all_keys, key_graph)
  total_keys = all_keys.size
  initial_positions = robot_positions.length.times.map { |i| "@#{i}" }
  heap = MinHeap.new
  heap.push([0, initial_positions, Set.new])
  visited = {}

  while !heap.empty?
    cost, positions, collected_keys = heap.pop
    
    state = [positions, bitmask(collected_keys)]
    next if visited[state] && visited[state] <= cost
    visited[state] = cost
    
    return cost if collected_keys.size == total_keys

    # For each robot, try to move to new keys
    positions.each_with_index do |pos, i|
      available_keys = []
      key_graph[pos].each do |key, (dist, required_keys)|
        if !collected_keys.include?(key) && required_keys.subset?(collected_keys)
          available_keys << [key, dist]
        end
      end

      available_keys.each do |key, dist|
        new_positions = positions.dup
        new_positions[i] = key
        new_collected_keys = collected_keys.clone
        new_collected_keys.add(key)
        new_state = [new_positions, bitmask(new_collected_keys)]
        next if visited[new_state] && visited[new_state] <= cost + dist
        heap.push([cost + dist, new_positions, new_collected_keys])
      end
    end
  end
  nil
end

result = dijkstra(robot_positions, all_keys, key_graph)
puts result
