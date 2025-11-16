
alias Graph = Hash(String, Set(String))

def parse_input(lines : Array(String)) : Graph
  graph = Graph.new
  lines.each do |line|
    vertex, others = line.split(": ")
    graph[vertex] = Set(String).new unless graph.has_key?(vertex)
    others.split.each do |other|
      graph[vertex] << other
      graph[other] = Set(String).new unless graph.has_key?(other)
      graph[other] << vertex
    end
  end
  graph
end

def breadth_first_search(graph : Graph, start : String, &goal : String -> Bool)
  frontier = Deque(String).new
  frontier << start
  reached = Set(String).new
  reached << start
  came_from = Hash(String, String).new
  came_from[start] = start
  until frontier.empty?
    current = frontier.shift
    return {true, came_from} if goal.call(current)
    graph[current]?.try &.each do |neighbor|
      next if reached.includes?(neighbor)
      frontier << neighbor
      reached << neighbor
      came_from[neighbor] = current
    end
  end
  {false, came_from}
end

def reconstruct_path(start : String, goal : String, came_from : Hash(String, String)) : Array(String)
  path = Array(String).new
  current = goal
  until current == start
    path << current
    current = came_from[current]
  end
  path << start
  path.reverse!
end

def solve(lines : Array(String)) : Int64
  min_cut = 3
  graph = parse_input(lines)
  source = graph.keys.first
  separate_graph = Graph.new
  graph.keys.each do |target|
    next if target == source
    new_graph = graph.dup
    new_graph.each do |k, v|
      new_graph[k] = v.dup
    end
    min_cut.times do
      found, came_from = breadth_first_search(new_graph, source) { |v| v == target }
      break unless found
      path = reconstruct_path(source, target, came_from)
      (path.size - 1).times do |j|
        u, v = path[j], path[j + 1]
        new_graph[u].delete(v)
        new_graph[v].delete(u)
      end
    end
    found, _ = breadth_first_search(new_graph, source) { |v| v == target }
    unless found
      separate_graph = new_graph
      break
    end
  end
  _, came_from_final = breadth_first_search(separate_graph, source) { false }
  length1 = came_from_final.size.to_i64
  length2 = graph.size.to_i64 - length1
  length1 * length2
end

lines = File.read_lines("input.txt").map(&.strip)
puts solve(lines)
