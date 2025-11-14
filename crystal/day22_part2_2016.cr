
require "set"

Neighbors4 = [[0, 1], [0, -1], [1, 0], [-1, 0]]
re = /-x(\d+)-y(\d+)/

struct Node
  getter used : Int32
  getter avail : Int32
  def initialize(@used, @avail); end
end

def dim(nodes)
  w = h = 0
  nodes.each_key { |pos| w = pos[0] if pos[0] > w; h = pos[1] if pos[1] > h }
  {w, h}
end

def find_hole(nodes)
  nodes.each { |pos, n| return pos if n.used == 0 }
  raise "no hole"
end

def moves(nodes, goal, from, to)
  w, h = dim(nodes)
  depth = {from => 0}
  pq = Deque(Tuple(Array(Int32), Int32)).new
  pq.push({from, 0})
  visited = Set.new([from])

  until pq.empty?
    p, _ = pq.shift
    if p == to
      return depth[p]
    end
    currdepth = depth[p] + 1
    Neighbors4.each do |n|
      next_pos = [p[0] + n[0], p[1] + n[1]]
      next if next_pos[0] < 0 || next_pos[1] < 0 || next_pos[0] > w || next_pos[1] > h || nodes[next_pos].used > 400 || next_pos == goal
      if !visited.includes?(next_pos) || currdepth < depth[next_pos]
        depth[next_pos] = currdepth
        pq.push({next_pos, -currdepth})
        visited.add(next_pos)
      end
    end
  end
  raise "no possible path"
end

def minmoves(nodes)
  w, _ = dim(nodes)
  goal = [w, 0]
  hole = find_hole(nodes)
  sum = 0

  until goal == [0, 0]
    next_pos = [goal[0] - 1, 0]
    m = moves(nodes, goal, hole, next_pos)
    sum += m
    hole = next_pos
    m = moves(nodes, goal, goal, hole)
    sum += m
    goal, hole = hole, goal
  end

  sum
end

nodes = Hash(Array(Int32), Node).new
File.read("input.txt").lines[2..].each do |line|
  f = line.split
  matches = re.match(f[0]).not_nil!
  pos = [matches[1].to_i, matches[2].to_i]
  nodes[pos] = Node.new(f[2][0..-2].to_i, f[3][0..-2].to_i)
end

puts minmoves(nodes)
