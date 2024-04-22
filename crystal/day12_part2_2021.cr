file = File.read("input.txt")
input = file.chomp

def solve(input)
  parsed = parse_input(input)

  graph = Hash(String, Hash(String, Bool)).new { |h, k| h[k] = Hash(String, Bool).new }
  parsed.each do |pair|
    graph[pair[0]] = {} of String => Bool unless graph[pair[0]]?
    graph[pair[1]] = {} of String => Bool unless graph[pair[1]]?
    graph[pair[0]][pair[1]] = true
    graph[pair[1]][pair[0]] = true
  end

  walk(graph, "start", Hash(String, Int32).new { |h, k| h[k] = 0 }, ["start"], false)
end

def walk(graph, current, visited, path, double_used)
  return 1 if current == "end"

  visited[current] += 1

  paths_to_end = 0

  graph[current].each_key do |visitable|
    next if visitable == "start"

    if visitable.downcase == visitable && visited[visitable] > 0
      if double_used
        next
      else
        double_used = true
      end
    end

    path << visitable
    paths_to_end += walk(graph, visitable, visited, path, double_used)

    visited[visitable] -= 1
    path.pop

    if visitable.downcase == visitable && visited[visitable] == 1
      double_used = false
    end
  end

  paths_to_end
end

def parse_input(input)
  input.split("\n").map { |line| line.split("-") }
end

puts solve(input)