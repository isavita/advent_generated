class Valve
  attr_accessor :id, :flow, :tunnels

  def initialize(id, flow)
    @id = id
    @flow = flow
    @tunnels = {}
  end
end

def read_all(path)
  File.read(path).strip
end

def max_pressure(valves, curr, minute, pressure, open, d)
  max = pressure
  open.each do |next_valve|
    new_open = open.reject { |v| v == next_valve }
    time_left = minute - valves[curr].tunnels[next_valve] - 1
    max = [max, max_pressure(valves, next_valve, time_left, time_left * valves[next_valve].flow + pressure, new_open, d + 1)].max if time_left > 0
  end
  max
end

def divide(l)
  return [[[], [0]], [[0], []]] if l == 1
  d = divide(l - 1)
  r = []
  d.each do |a, b|
    r << [[l - 1] + a, b]
    r << [a, [l - 1] + b]
  end
  r
end

input = read_all('input.txt')
valves = {}
input.each_line do |line|
  sp = line.split('; ')
  id, flow = sp[0].match(/Valve (\w+) has flow rate=(\d+)/).captures
  v = Valve.new(id, flow.to_i)
  sp[1] = sp[1][/tunnel(s)? lead(s)? to valve(s)? (.+)/, 4]
  sp[1].split(', ').each { |t| v.tunnels[t] = 1 }
  valves[id] = v
end

valves.each_key do |k|
  valves.each_key do |i|
    valves.each_key do |j|
      dik = valves[i].tunnels[k]
      dkj = valves[k].tunnels[j]
      if dik && dkj
        dij = valves[i].tunnels[j]
        valves[i].tunnels[j] = [dij || Float::INFINITY, dik + dkj].min
      end
    end
  end
end

open = valves.select { |_, v| v.flow > 0 }.keys

puts max_pressure(valves, 'AA', 30, 0, open, 0)