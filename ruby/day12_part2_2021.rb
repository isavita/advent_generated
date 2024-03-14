def solve(input)
  parsed = parse_input(input)

  graph = {}
  parsed.each do |pair|
    graph[pair[0]] = {} unless graph.key?(pair[0])
    graph[pair[1]] = {} unless graph.key?(pair[1])
    graph[pair[0]][pair[1]] = true
    graph[pair[1]][pair[0]] = true
  end

  walk(graph, "start", { "start" => 1 }, ["start"], false)
end

def walk(graph, current, visited, path, double_used)
  return 1 if current == "end"

  paths_to_end = 0

  graph[current].each do |visitable, _|
    next if visitable == "start"

    if visitable.downcase == visitable && (visited[visitable] || 0) > 0
      next if double_used
      double_used = true
    end

    path << visitable
    visited[visitable] = (visited[visitable] || 0) + 1
    paths_to_end += walk(graph, visitable, visited, path, double_used)
    visited[visitable] -= 1
    path.pop

    double_used = false if visitable.downcase == visitable && visited[visitable] == 1
  end

  paths_to_end
end

def parse_input(input)
  input.split("\n").map { |line| line.split("-") }
end

File.open("input.txt", "r") do |file|
  input = file.read.strip
  puts solve(input)
end