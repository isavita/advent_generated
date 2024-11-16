
class Valve
  property id : String
  property flow : Int32
  property tunnels : Hash(String, Int32)

  def initialize(@id, @flow, @tunnels)
  end
end

def max_pressure(valves : Hash(String, Valve), curr : String, minute : Int32, pressure : Int32, open : Array(String), depth : Int32) : Int32
  max = pressure
  open.each do |next_valve|
    new_open = open.reject { |v| v == next_valve }
    time_left = minute - valves[curr].tunnels[next_valve] - 1
    if time_left > 0
      new_pressure = time_left * valves[next_valve].flow + pressure
      max = {max, max_pressure(valves, next_valve, time_left, new_pressure, new_open, depth + 1)}.max
    end
  end
  max
end

def read_all(path : String) : String
  File.read(path).strip
end

def main
  valves = {} of String => Valve

  input = read_all("input.txt")
  input.split('\n').each do |line|
    parts = line.split("; ")
    id, flow = parts[0].match(/Valve (\w+) has flow rate=(\d+)/).not_nil![1, 2]
    flow = flow.to_i

    tunnels_str = parts[1].sub(/tunnel(s)? lead(s)? to valve(s)? /, "")
    tunnels = {id => 0}.merge(tunnels_str.split(", ").map { |t| {t, 1} }.to_h)

    valves[id] = Valve.new(id, flow, tunnels)
  end

  # Floyd-Warshall algorithm for shortest paths
  valves.keys.each do |k|
    valves.keys.each do |i|
      valves.keys.each do |j|
        if valves[i].tunnels.has_key?(k) && valves[k].tunnels.has_key?(j)
          dik = valves[i].tunnels[k]
          dkj = valves[k].tunnels[j]
          dij = valves[i].tunnels[j]?

          if dij.nil? || dij > dik + dkj
            valves[i].tunnels[j] = dik + dkj
          end
        end
      end
    end
  end

  open = valves.values.select { |v| v.flow > 0 }.map(&.id)

  puts max_pressure(valves, "AA", 30, 0, open, 0)
end

main
