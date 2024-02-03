
class Cave
  attr_accessor :connections

  def initialize
    @connections = {}
  end

  def connect_to(name)
    @connections[name] = true
  end

  def disconnect_from(name)
    @connections.delete(name)
  end
end

caves = {}

File.open("input.txt").each do |line|
  paths = line.chomp.split("-")
  from, to = paths[0], paths[1]

  caves[from] ||= Cave.new
  caves[to] ||= Cave.new

  caves[from].connect_to(to)
  caves[to].connect_to(from)
end

count = 0
dfs = lambda do |current, visited|
  if current == "end"
    count += 1
    return
  end

  caves[current].connections.each do |next_cave, _|
    if visited[next_cave] && next_cave.downcase == next_cave
      next
    end

    visited_copy = visited.clone
    visited_copy[next_cave] = true
    dfs.call(next_cave, visited_copy)
  end
end

dfs.call("start", {"start" => true})
puts count
