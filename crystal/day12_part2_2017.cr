require "file_utils"

file = File.open("input.txt")
adj = Hash(Int32, Array(Int32)).new
visited = Hash(Int32, Bool).new

file.each_line do |line|
  parts = line.split(" <-> ")
  from = parts[0].to_i
  to_nodes = parts[1].split(", ")
  to_nodes.each do |to_node|
    to = to_node.to_i
    adj[from] = adj.fetch(from, [] of Int32) << to
    adj[to] = adj.fetch(to, [] of Int32) << from
  end
end

def dfs(node, adj, visited)
  visited[node] = true
  adj.fetch(node, [] of Int32).each do |neighbor|
    dfs(neighbor, adj, visited) unless visited[neighbor]?
  end
end

groups = 0
adj.each_key do |node|
  unless visited[node]?
    dfs(node, adj, visited)
    groups += 1
  end
end

puts groups