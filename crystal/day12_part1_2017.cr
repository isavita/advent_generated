# DFS function to traverse the connections
def dfs(node, adj, visited)
  visited[node] = true
  adj[node].each do |neighbor|
    dfs(neighbor, adj, visited) unless visited[neighbor]
  end
end

# Read input from file
File.open("input.txt") do |file|
  # Adjacency list to store connections
  adj = Hash(Int32, Array(Int32)).new { |h, k| h[k] = [] of Int32 }

  file.each_line do |line|
    parts = line.chomp.split(" <-> ")
    from = parts[0].to_i
    to_nodes = parts[1].split(", ").map(&.to_i)

    to_nodes.each do |to|
      adj[from] << to
      adj[to] << from
    end
  end

  # Set to keep track of visited nodes
  visited = Hash(Int32, Bool).new { |h, k| h[k] = false }

  # Start DFS from node 0
  dfs(0, adj, visited)

  # Count the number of visited nodes
  count = visited.count { |_, v| v }

  puts count
end