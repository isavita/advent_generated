
def max(a, b)
  a > b ? a : b
end

def max_pressure(valves, curr, minute, pressure, open, distances)
  max_p = pressure
  open.each do |next_v|
    new_open = open.reject { |v| v == next_v }
    time_left = minute - distances[curr][next_v] - 1
    if time_left > 0
      max_p = max(max_p, max_pressure(valves, next_v, time_left, time_left * valves[next_v][:flow] + pressure, new_open, distances))
    end
  end
  max_p
end

valves = {}
input = File.read('input.txt').strip
input.split("\n").each do |line|
  sp = line.split('; ')
  v = {}
  sp[0] =~ /Valve (\w+) has flow rate=(\d+)/
  v[:id] = $1
  v[:flow] = $2.to_i
  sp[1] = sp[1][sp[1].index('valve')..][5..]
  if sp[1].start_with?('s')
    sp[1] = sp[1][2..]
  else
    sp[1] = sp[1][1..]
  end
  v[:tunnels] = sp[1].split(', ')
  valves[v[:id]] = v
end

distances = Hash.new { |h, k| h[k] = {} }
valves.each_key do |i|
  valves.each_key do |j|
    distances[i][j] = (i == j ? 0 : Float::INFINITY)
  end
end

valves.each do |k, v|
  v[:tunnels].each do |j|
    distances[k][j] = 1
  end
end

valves.each_key do |k|
  valves.each_key do |i|
    valves.each_key do |j|
      if distances[i][k] + distances[k][j] < distances[i][j]
        distances[i][j] = distances[i][k] + distances[k][j]
      end
    end
  end
end

open = valves.select { |_, v| v[:flow] > 0 }.keys
max_pressure_total = 0

(0..2**open.length - 1).each do |mask|
  mine = []
  elephant = []
  open.each_with_index do |valve, index|
    if (mask >> index) & 1 == 1
      mine << valve
    else
      elephant << valve
    end
  end
  next if mine.empty? || elephant.empty?

  x = max_pressure(valves, 'AA', 26, 0, mine, distances) + max_pressure(valves, 'AA', 26, 0, elephant, distances)
  max_pressure_total = max(max_pressure_total, x)
end

puts max_pressure_total
