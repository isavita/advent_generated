
# Read the input from a file called input.txt
def read_input(file_path : String)
  File.read_lines(file_path)
end

# Build a graph from the input connections
def build_graph(connections : Array(String))
  graph = Hash(String, Array(String)).new

  connections.each do |connection|
    a, b = connection.split("-")
    graph[a] ||= [] of String
    graph[b] ||= [] of String
    graph[a] << b
    graph[b] << a
  end

  graph
end

# Perform a depth-first search to count paths
def count_paths(graph : Hash(String, Array(String)), current : String, visited : Set(String), can_visit_twice : Bool)
  return 1 if current == "end"
  return 0 if visited.includes?(current) && (current.downcase == current) # Small cave visited twice

  visited.add(current)
  path_count = 0

  graph[current].each do |neighbor|
    if neighbor == "start" && visited.includes?(neighbor)
      next # Don't revisit start
    end

    if visited.includes?(neighbor) && (neighbor.downcase == neighbor) && !can_visit_twice
      # If it's a small cave and already visited, we can't visit it again
      next
    end

    # If it's a small cave already visited, we can only visit it again if we haven't used our "twice" option
    if visited.includes?(neighbor) && (neighbor.downcase == neighbor)
      path_count += count_paths(graph, neighbor, visited.clone, false) # Use the option to visit twice
    else
      path_count += count_paths(graph, neighbor, visited, can_visit_twice)
    end
  end

  visited.delete(current) # Backtrack
  path_count
end

# Main execution
def main
  connections = read_input("input.txt")
  graph = build_graph(connections)
  total_paths = count_paths(graph, "start", Set(String).new, true)
  puts total_paths
end

main
