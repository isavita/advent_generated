
# frozen_string_literal: true

def dfs(u : Int32, target : Int32, edges : Array(Array(Int32)), memo : Array(Int64)) : Int64
  return 1_i64 if u == target
  return memo[u] if memo[u] != -1_i64

  total = 0_i64
  edges[u].each do |v|
    total += dfs(v, target, edges, memo)
  end
  memo[u] = total
end

node_index = {} of String => Int32
edges = [] of Array(Int32)

def get_index(name : String, node_index : Hash(String, Int32), edges : Array(Array(Int32))) : Int32
  idx = node_index[name]?
  return idx if idx

  new_idx = node_index.size
  node_index[name] = new_idx
  edges << [] of Int32
  new_idx
end

File.each_line("input.txt") do |line|
  src_part, rest = line.split(':', 2)
  next unless rest

  src = src_part.strip
  u = get_index(src, node_index, edges)

  rest.split.each do |dest|
    v = get_index(dest, node_index, edges)
    edges[u] << v
  end
end

memo = Array.new(node_index.size, -1_i64)

start = get_index("you", node_index, edges)
finish = get_index("out", node_index, edges)

puts dfs(start, finish, edges, memo)
