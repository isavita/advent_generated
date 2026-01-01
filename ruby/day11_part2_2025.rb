
#!/usr/bin/env ruby
require 'set'

ids = {}
graph = []

idx = lambda do |s|
  ids[s] ||= begin
    i = ids.size
    graph << []
    i
  end
end

add_edge = lambda do |u, v|
  graph[u] << v
end

def dfs(cur, tgt, graph, memo)
  return 1 if cur == tgt
  return memo[cur] if memo[cur] != -1
  sum = 0
  graph[cur].each { |n| sum += dfs(n, tgt, graph, memo) }
  memo[cur] = sum
  sum
end

def count(s, t, graph)
  memo = Array.new(graph.size, -1)
  dfs(s, t, graph, memo)
end

if __FILE__ == $0
  File.foreach('input.txt') do |line|
    line.strip!
    next if line.empty?
    parts = line.split(':', 2)
    next unless parts.size == 2
    u = idx.call(parts[0].strip)
    parts[1].strip.split(/\s+/).each { |v| add_edge.call(u, idx.call(v)) }
  end

  svr = idx.call('svr')
  dac = idx.call('dac')
  fft = idx.call('fft')
  out = idx.call('out')

  s1 = count(svr, dac, graph) * count(dac, fft, graph) * count(fft, out, graph)
  s2 = count(svr, fft, graph) * count(fft, dac, graph) * count(dac, out, graph)

  puts "Paths (svr->dac->fft->out): #{s1}"
  puts "Paths (svr->fft->dac->out): #{s2}"
  puts "Total paths visiting both: #{s1 + s2}"
end
