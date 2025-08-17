
#!/usr/bin/env crystal
require "big"

struct Rule
  getter x : Int32
  getter y : Int32
  def initialize(@x : Int32, @y : Int32); end
end

def correct?(pages : Array(Int32), rules : Array(Rule)) : Bool
  pos = {} of Int32 => Int32
  pages.each_with_index { |p, i| pos[p] = i }
  rules.each do |r|
    if (px = pos[r.x]?) && (py = pos[r.y]?) && px > py
      return false
    end
  end
  true
end

def topological(pages : Array(Int32), rules : Array(Rule)) : Array(Int32)
  indeg = {} of Int32 => Int32
  adj = {} of Int32 => Array(Int32)
  pages.each do |p|
    indeg[p] = 0
    adj[p] = [] of Int32
  end
  rules.each do |r|
    next unless indeg.has_key?(r.x) && indeg.has_key?(r.y)
    adj[r.x] << r.y
    indeg[r.y] = indeg[r.y] + 1
  end
  q = [] of Int32
  indeg.each { |k, v| q << k if v == 0 }
  order = [] of Int32
  until q.empty?
    u = q.shift
    order << u
    adj[u].each do |v|
      indeg[v] -= 1
      q << v if indeg[v] == 0
    end
  end
  order
end

rules = [] of Rule
updates = [] of Array(Int32)
reading_rules = true

File.each_line("input.txt") do |line|
  line = line.strip
  next if line.empty?
  if line.includes?('|')
    reading_rules = true
    x, y = line.split('|', 2).map(&.to_i)
    rules << Rule.new(x, y)
  else
    reading_rules = false
    updates << line.split(',').map(&.to_i)
  end
end

sum = 0_i64
updates.each do |pages|
  unless correct?(pages, rules)
    order = topological(pages, rules)
    sum += order[order.size // 2] if order.size > 0
  end
end

puts sum
