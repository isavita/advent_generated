#!/usr/bin/env ruby
def count_paths(cur, target, adj, memo)
  return 1 if cur == target
  return memo[cur] if memo.key?(cur)
  total = 0
  adj[cur].each { |nbr| total += count_paths(nbr, target, adj, memo) }
  memo[cur] = total
  total
end

if __FILE__ == $0
  adj = Hash.new { |h, k| h[k] = [] }
  File.foreach('input.txt') do |line|
    line = line.strip
    next if line.empty?
    parts = line.split(':', 2)
    next unless parts.size == 2
    source = parts[0].strip
    dests = parts[1].strip.split(/\s+/)
    adj[source] = dests
  end
  memo = {}
  count = count_paths('you', 'out', adj, memo)
  puts "Number of paths from 'you' to 'out': #{count}"
end