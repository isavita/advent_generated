#!/usr/bin/env crystal
if !File.file?("input.txt")
  exit
end

adj = Hash(String, Array(String)).new { |h, k| h[k] = [] of String }
File.each_line("input.txt") do |line|
  line = line.strip
  next if line.empty? || !line.includes?(":")
  node, rest = line.split(":", 2)
  node = node.strip
  targets = rest.strip.split(/\s+/)
  adj[node].concat(targets)
end

memo = Hash(String, Int64).new

def count_paths(cur, target, adj, memo) : Int64
  return 1_i64 if cur == target
  key = "#{cur}->#{target}"
  return memo[key] if memo.has_key?(key)
  count = 0_i64
  if neighbors = adj[cur]?
    neighbors.each { |n| count += count_paths(n, target, adj, memo) }
  end
  memo[key] = count
  count
end

path1 = count_paths("svr", "dac", adj, memo) *
        count_paths("dac", "fft", adj, memo) *
        count_paths("fft", "out", adj, memo)

path2 = count_paths("svr", "fft", adj, memo) *
        count_paths("fft", "dac", adj, memo) *
        count_paths("dac", "out", adj, memo)

puts path1 + path2