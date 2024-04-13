class Edge
  attr_accessor :start, :endpoint, :weight

  def initialize(start, endpoint, weight)
    @start = start
    @endpoint = endpoint
    @weight = weight
  end

  def eql?(other)
    self.start == other.start && self.endpoint == other.endpoint && self.weight == other.weight
  end

  def hash
    [@start, @endpoint, @weight].hash
  end
end

def parse_input(input)
  weight = 1
  graph = {}

  input.each do |line|
    parts = line.split(": ")
    vertice = parts[0]
    others = parts[1].split(" ")

    graph[vertice] ||= {}

    others.each do |other|
      graph[other] ||= {}
      edge = Edge.new(vertice, other, weight)
      reverse_edge = Edge.new(other, vertice, weight)
      graph[vertice][edge] = nil
      graph[other][reverse_edge] = nil
    end
  end

  graph
end

def breadth_first_search(graph, start, goal_func)
  frontier = [start]
  reached = {start => nil}
  came_from = {start => start}

  until frontier.empty?
    current = frontier.shift
    return [true, came_from] if goal_func.call(current)

    graph[current].keys.each do |edge|
      unless reached.key?(edge.endpoint)
        frontier.push(edge.endpoint)
        reached[edge.endpoint] = nil
        came_from[edge.endpoint] = current
      end
    end
  end

  [false, came_from]
end

def reconstruct_path(start, end_, came_from)
  path = []
  current = end_
  while current != start
    path.unshift(current)
    current = came_from[current]
  end
  path.unshift(start)
  path
end

def copy_graph(graph)
  new_graph = {}
  graph.each do |vertice, edges|
    new_graph[vertice] = {}
    edges.each_key do |edge|
      new_graph[vertice][edge] = nil
    end
  end
  new_graph
end

def solve(input)
  min_cut = 3
  graph = parse_input(input)

  source = graph.keys.first
  separate_graph = nil

  graph.keys.each do |end_|
    next if source == end_

    new_graph = copy_graph(graph)
    min_cut.times do
      _, came_from = breadth_first_search(new_graph, source, ->(v) { v == end_ })
      path = reconstruct_path(source, end_, came_from)
      (path.size - 1).times do |j|
        edge = Edge.new(path[j], path[j+1], 1)
        new_graph[path[j]].delete(edge)
      end
    end

    valid, _ = breadth_first_search(new_graph, source, ->(v) { v == end_ })
    unless valid
      separate_graph = new_graph
      break
    end
  end

  _, came_from = breadth_first_search(separate_graph, source, ->(_) { false })
  length1 = came_from.size
  length2 = separate_graph.size - length1

  length1 * length2
end

def read_file(file_name)
  File.read(file_name).strip.split("\n")
end

input = read_file("input.txt")
puts solve(input)