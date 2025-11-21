
require "json"

struct Valve
  property id : String
  property flow : Int32
  property tunnels : Hash(String, Int32)

  def initialize(@id : String, @flow : Int32)
    @tunnels = Hash(String, Int32).new
    @tunnels[@id] = 0
  end
end

def read_input : String
  File.read("input.txt").strip
end

def parse_input(text : String) : Hash(String, Valve)
  valves = Hash(String, Valve).new
  text.each_line do |line|
    parts = line.split("; ")
    m = /Valve (\w+) has flow rate=(\d+)/.match(parts[0])
    next unless m
    id = m[1]
    flow = m[2].to_i
    valve = Valve.new(id, flow)
    tunnels_str = parts[1][parts[1].index("valve").not_nil! + 5..].strip
    tunnels_str = tunnels_str[2..] if tunnels_str.starts_with?('s')
    tunnels_str.split(", ").each { |t| valve.tunnels[t] = 1 }
    valves[id] = valve
  end
  valves
end

def floyd_warshall(valves : Hash(String, Valve))
  keys = valves.keys
  keys.each do |k|
    keys.each do |i|
      keys.each do |j|
        di = valves[i].tunnels[k]?
        dk = valves[k].tunnels[j]?
        next if di.nil? || dk.nil?
        nd = di + dk
        if (dj = valves[i].tunnels[j]?) && dj <= nd
          next
        end
        valves[i].tunnels[j] = nd
      end
    end
  end
end

def open_valves(valves : Hash(String, Valve)) : Array(String)
  valves.values.select(&.flow.> 0).map(&.id)
end

def max_pressure(valves : Hash(String, Valve), cur : String, minutes : Int32,
                 pressure : Int32, open : Array(String)) : Int32
  max = pressure
  open.each_with_index do |next_id, idx|
    time_left = minutes - valves[cur].tunnels[next_id] - 1
    next if time_left <= 0
    new_open = open.dup
    new_open.delete_at(idx)
    gained = time_left * valves[next_id].flow
    cand = max_pressure(valves, next_id, time_left, pressure + gained, new_open)
    max = cand if cand > max
  end
  max
end

def divide(len : Int32) : Array({Array(Int32), Array(Int32)})
  return [{[] of Int32, [0]}, {[0], [] of Int32}] if len == 1
  prev = divide(len - 1)
  res = [] of {Array(Int32), Array(Int32)}
  prev.each do |a, b|
    res << {[len - 1] + a, b}
    res << {a, [len - 1] + b}
  end
  res
end

def main
  valves = parse_input(read_input)
  floyd_warshall(valves)
  open = open_valves(valves)

  best = 0
  divide(open.size).each do |mine_idx, ele_idx|
    next if mine_idx.empty? || ele_idx.empty?
    mine = mine_idx.map { |i| open[i] }
    ele  = ele_idx.map { |i| open[i] }
    p1 = max_pressure(valves, "AA", 26, 0, mine)
    p2 = max_pressure(valves, "AA", 26, 0, ele)
    total = p1 + p2
    best = total if total > best
  end
  puts best
end

main
