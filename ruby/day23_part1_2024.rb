
def solve
  graph = Hash.new { |h, k| h[k] = {} }
  File.readlines("input.txt").each do |line|
    a, b = line.strip.split("-")
    next unless a && b
    graph[a][b] = true
    graph[b][a] = true
  end

  triplets = Set.new
  computers = graph.keys
  computers.size.times do |i|
    (i + 1...computers.size).each do |j|
      (j + 1...computers.size).each do |k|
        c1, c2, c3 = computers[i], computers[j], computers[k]
        if graph[c1][c2] && graph[c2][c3] && graph[c1][c3]
           if c1.start_with?("t") || c2.start_with?("t") || c3.start_with?("t")
             triplets << [c1,c2,c3].sort.join(",")
           end
        end
      end
    end
  end
  puts triplets.size
end

require 'set'
solve
