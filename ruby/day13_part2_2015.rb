
def read_happiness_values(filename)
  happiness_map = Hash.new { |h, k| h[k] = {} }
  File.readlines(filename).each do |line|
    parts = line.split
    next if parts.size < 11
    from, to = parts[0], parts[10].chomp('.')
    change = parts[3].to_i
    change = -change if parts[2] == 'lose'
    happiness_map[from][to] = change
  end
  happiness_map
end

def add_yourself(happiness_map)
  happiness_map['You'] = {}
  happiness_map.each do |guest, _|
    happiness_map[guest]['You'] = 0
    happiness_map['You'][guest] = 0
  end
end

def calculate_optimal_arrangement(happiness_map)
  guests = happiness_map.keys
  guests.permutation.map { |arrangement|
    arrangement.each_with_index.sum { |guest, i|
      left = arrangement[(i - 1) % guests.size]
      right = arrangement[(i + 1) % guests.size]
      happiness_map[guest][left] + happiness_map[guest][right]
    }
  }.max
end

happiness_map = read_happiness_values('input.txt')
add_yourself(happiness_map)
max_happiness = calculate_optimal_arrangement(happiness_map)
puts max_happiness
