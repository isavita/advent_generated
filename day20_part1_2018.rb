
input = File.read('input.txt').strip

def parse_regex(input)
  stack = []
  graph = Hash.new { |h, k| h[k] = [] }
  current = [0, 0]
  distance = { current => 0 }

  input.each_char do |char|
    case char
    when 'N'
      new_pos = [current[0], current[1] + 1]
    when 'S'
      new_pos = [current[0], current[1] - 1]
    when 'E'
      new_pos = [current[0] + 1, current[1]]
    when 'W'
      new_pos = [current[0] - 1, current[1]]
    when '('
      stack.push(current)
    when '|'
      current = stack.last
    when ')'
      current = stack.pop
    end

    if %w(N S E W).include?(char)
      graph[current] << new_pos
      graph[new_pos] << current
      distance[new_pos] = distance[current] + 1
      distance[current] = [distance[current], distance[new_pos]].min
      current = new_pos
    end
  end

  [graph, distance]
end

def bfs(graph, start)
  queue = [[start, 0]]
  visited = { start => 0 }
  max_distance = 0

  until queue.empty?
    current, dist = queue.shift
    max_distance = [max_distance, dist].max

    graph[current].each do |neighbor|
      next if visited.key?(neighbor)

      visited[neighbor] = dist + 1
      queue << [neighbor, dist + 1]
    end
  end

  max_distance
end

graph, distance = parse_regex(input)
start = [0, 0]
result = bfs(graph, start)

puts result
