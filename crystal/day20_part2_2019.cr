
struct P
  getter x : Int32
  getter y : Int32
  def initialize(@x,@y) end
  def neighbors : Array(P)
    [P.new(x,y+1),P.new(x+1,y),P.new(x,y-1),P.new(x-1,y)]
  end
end
record Status, p : P, depth : Int32
grid = Hash(P, Char).new
lines = File.read_lines("input.txt")
xmax = lines.size
ymax = lines.map(&.size).max
lines.each_with_index do |line,i|
  line.chars.each_with_index do |c,j|
    grid[P.new(i,j)] = c
  end
end
aa = zz = P.new(0,0)
is_outer = Hash(P, Bool).new
portal_name = Hash(P, String).new
teleport = Hash(P, P).new
cache = Hash(String, P).new
def extract_portal(grid, p)
  c1 = grid[p]? || ' '
  if (grid[P.new(p.x+1,p.y)]?.try &.letter?)
    name = "#{c1}#{grid[P.new(p.x+1,p.y)]}"
    p1 = P.new(p.x+2,p.y)
    return {name,p1,true} if grid[p1]? == '.'
    p2 = P.new(p.x-1,p.y)
    return {name,p2,true} if grid[p2]? == '.'
  end
  if (grid[P.new(p.x,p.y+1)]?.try &.letter?)
    name = "#{c1}#{grid[P.new(p.x,p.y+1)]}"
    p1 = P.new(p.x,p.y+2)
    return {name,p1,true} if grid[p1]? == '.'
    p2 = P.new(p.x,p.y-1)
    return {name,p2,true} if grid[p2]? == '.'
  end
  {"", P.new(0,0), false}
end
grid.each do |p,c|
  next unless c.letter?
  name, point, ok = extract_portal(grid, p)
  next unless ok
  portal_name[point] = name
  if name == "AA"
    aa = point
    is_outer[point] = true
  elsif name == "ZZ"
    zz = point
    is_outer[point] = true
  else
    if cache.has_key?(name)
      target = cache[name]
      teleport[point] = target
      teleport[target] = point
    else
      cache[name] = point
    end
    is_outer[point] = (p.y == 0 || p.x == 0 || p.x == xmax-2 || p.y == ymax-2)
  end
end
discovered = Set(Status).new
todo = Deque(Status).new
root = Status.new(aa,0)
discovered << root
todo << root
steps = 0
while todo.any?
  lvl = todo.size
  lvl.times do
    curr = todo.shift
    curr.p.neighbors.each do |n|
      case grid[n]?
      when '#'
      when '.'
        target = Status.new(n,curr.depth)
        unless discovered.includes?(target)
          discovered << target
          todo << target
        end
      when .try &.letter?
        outer = is_outer[curr.p]?
        target = if !outer
          Status.new(teleport[curr.p], curr.depth+1)
        else
          name = portal_name[curr.p]
          if curr.depth == 0
            name == "ZZ" ? (puts steps; exit) : Status.new(P.new(-1,-1),-1)
          elsif name == "AA" || name == "ZZ"
            Status.new(P.new(-1,-1),-1)
          else
            Status.new(teleport[curr.p], curr.depth-1)
          end
        end
        if target.p.x != -1 && !discovered.includes?(target)
          discovered << target
          todo << target
        end
      end
    end
  end
  steps += 1
end
puts -1
