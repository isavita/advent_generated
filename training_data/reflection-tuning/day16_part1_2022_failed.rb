require 'set'

class Valve
  attr_reader :name, :flow_rate, :tunnels
  def initialize(name, flow_rate, tunnels)
    @name = name
    @flow_rate = flow_rate
    @tunnels = tunnels
  end
end

def parse_input(filename)
  valves = {}
  File.readlines(filename, chomp: true).each do |line|
    name = line[6, 2]
    flow_rate = line.scan(/\d+/).first.to_i
    tunnels = line.scan(/[A-Z]{2}/).drop(1)
    valves[name] = Valve.new(name, flow_rate, tunnels)
  end
  valves
end

def dfs(valve, time_left, opened, valves, memo)
  state = [valve, time_left, opened]
  return memo[state] if memo.key?(state)

  max_pressure = 0

  if time_left > 0 && !opened.include?(valve) && valves[valve].flow_rate > 0
    new_opened = opened + [valve]
    pressure = valves[valve].flow_rate * (time_left - 1)
    pressure += dfs(valve, time_left - 1, new_opened, valves, memo)
    max_pressure = [max_pressure, pressure].max
  end

  valves[valve].tunnels.each do |next_valve|
    pressure = dfs(next_valve, time_left - 1, opened, valves, memo)
    max_pressure = [max_pressure, pressure].max
  end

  memo[state] = max_pressure
end

valves = parse_input('input.txt')
memo = {}
max_pressure = dfs('AA', 30, [], valves, memo)
puts max_pressure
