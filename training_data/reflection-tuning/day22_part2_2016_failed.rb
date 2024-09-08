class Node
  attr_reader :x, :y, :size, :used, :avail
  def initialize(x, y, size, used)
    @x, @y, @size, @used = x, y, size, used
    @avail = size - used
  end
end

def parse_input(input)
  nodes = []
  input.each_line do |line|
    if line =~ /node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T/
      nodes << Node.new($1.to_i, $2.to_i, $3.to_i, $4.to_i)
    end
  end
  nodes
end

def count_viable_pairs(nodes)
  viable_pairs = 0
  nodes.combination(2) do |a, b|
    viable_pairs += 1 if a.used > 0 && a.used <= b.avail
    viable_pairs += 1 if b.used > 0 && b.used <= a.avail
  end
  viable_pairs
end

def find_path(nodes)
  grid = nodes.group_by(&:y).transform_values { |row| row.sort_by(&:x) }
  max_x = grid.values.first.last.x
  empty_node = nodes.find { |n| n.used == 0 }
  goal_node = grid[0][max_x]

  steps = 0
  current = empty_node

  # Move empty node to the left of the goal node
  while current.x < max_x - 1 || current.y > 0
    if current.x < max_x - 1
      current = grid[current.y][current.x + 1]
    else
      current = grid[current.y - 1][current.x]
    end
    steps += 1
  end

  # Move goal data to the left
  steps += 5 * (max_x - 1)

  steps
end

input = File.read('input.txt')
nodes = parse_input(input)

puts "Part 1: #{count_viable_pairs(nodes)}"
puts "Part 2: #{find_path(nodes)}"
