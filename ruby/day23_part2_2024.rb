
def solve
  graph = Hash.new { |h, k| h[k] = {} }
  nodes = Set.new
  File.foreach("input.txt") do |line|
    a, b = line.strip.split("-")
    next unless a && b
    graph[a][b] = true
    graph[b][a] = true
    nodes << a
    nodes << b
  end

  best_clique = []
  bron_kerbosch([], nodes.to_a, [], graph, best_clique)
  puts best_clique.sort.join(",")
end

def bron_kerbosch(r, p, x, graph, best_clique)
  if p.empty? && x.empty?
    if r.length > best_clique.length
      best_clique.replace(r)
    end
    return
  end

  p.dup.each do |v|
    neighbors = graph[v]
    bron_kerbosch(
      r + [v],
      p.select { |node| neighbors[node] },
      x.select { |node| neighbors[node] },
      graph,
      best_clique
    )
     p.delete(v)
     x << v
  end
end

require 'set'
solve
