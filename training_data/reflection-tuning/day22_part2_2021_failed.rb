class SegmentTree
  attr_reader :tree, :lazy

  def initialize(start, stop)
    @start = start
    @stop = stop
    @tree = Array.new(4 * (stop - start + 1), 0)
    @lazy = Array.new(4 * (stop - start + 1), 0)
  end

  def update(l, r, val, node = 1, start = @start, stop = @stop)
    if l > stop || r < start
      return
    end
    if l <= start && stop <= r
      @tree[node] += val * (stop - start + 1)
      @lazy[node] += val
      return
    end
    mid = (start + stop) / 2
    update(l, r, val, 2 * node, start, mid)
    update(l, r, val, 2 * node + 1, mid + 1, stop)
    @tree[node] = @tree[2 * node] + @tree[2 * node + 1] + @lazy[node] * (stop - start + 1)
  end

  def query
    @tree[1]
  end
end

def solve(steps)
  events = []
  coords = { x: Set.new, y: Set.new, z: Set.new }

  steps.each do |on, x1, x2, y1, y2, z1, z2|
    [x1, x2 + 1, y1, y2 + 1, z1, z2 + 1].each_slice(2).zip([:x, :y, :z]) do |(start, stop), axis|
      coords[axis].add(start)
      coords[axis].add(stop)
    end
    events << [x1, y1, z1, y2, z2, on ? 1 : -1]
    events << [x2 + 1, y1, z1, y2, z2, on ? -1 : 1]
  end

  [:x, :y, :z].each { |axis| coords[axis] = coords[axis].sort.each_with_index.to_h }

  events.sort_by! { |x, *_| x }

  tree = SegmentTree.new(0, coords[:z].size - 1)
  prev_x = events.first[0]
  total_volume = 0

  events.each do |x, y1, z1, y2, z2, val|
    total_volume += tree.query * (coords[:x][x] - coords[:x][prev_x])
    tree.update(coords[:z][z1], coords[:z][z2] - 1, val * (coords[:y][y2] - coords[:y][y1]))
    prev_x = x
  end

  total_volume
end

steps = File.readlines('input.txt').map do |line|
  on, coords = line.strip.split
  on = (on == 'on')
  coords = coords.split(',').flat_map { |c| c.split('=')[1].split('..').map(&:to_i) }
  [on] + coords
end

puts solve(steps)
