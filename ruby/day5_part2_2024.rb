
def read_input(filename)
  ordering_rules, updates = [], []
  File.open(filename).each_line do |line|
    line.strip!
    next if line.empty?

    if line.include?('|')
      x, y = line.split('|').map(&:strip).map(&:to_i)
      ordering_rules << [x, y]
    else
      updates << line.split(',').map(&:strip).map(&:to_i)
    end
  end
  [ordering_rules, updates]
end

def is_correctly_ordered?(update, rules)
  position = update.each_with_index.to_h
  rules.all? do |x, y|
    !position.key?(x) || !position.key?(y) || position[x] < position[y]
  end
end

def topological_sort(update, rules)
  adjacency = {}
  update.each { |page| adjacency[page] = [] }
  rules.each do |x, y|
    adjacency[x] << y if update.include?(x) && update.include?(y)
  end

  visited = {}
  result = []
  
  def visit(node, adjacency, visited, result, &block)
    return if visited[node]
    visited[node] = true
    adjacency[node].each { |neighbor| visit(neighbor, adjacency, visited, result, &block) }
    result.unshift(node)
  end

  update.each { |node| visit(node, adjacency, visited, result) }
  result
end


ordering_rules, updates = read_input("input.txt")

sum = 0
updates.each do |update|
  unless is_correctly_ordered?(update, ordering_rules)
    sorted_update = topological_sort(update, ordering_rules)
    sum += sorted_update[sorted_update.length / 2]
  end
end

puts sum
