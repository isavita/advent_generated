
def read_happiness_values(filename)
  happiness_map = Hash.new { |h, k| h[k] = {} }
  File.foreach(filename) do |line|
    parts = line.split
    next if parts.size < 11
    from, to = parts[0], parts[10].chomp('.')
    change = parts[3].to_i
    change = -change if parts[2] == 'lose'
    happiness_map[from][to] = change
  end
  happiness_map
end

def calculate_happiness(arrangement, happiness_map)
  n = arrangement.size
  arrangement.each_with_index.sum do |person, i|
    left = arrangement[(i - 1) % n]
    right = arrangement[(i + 1) % n]
    happiness_map[person][left] + happiness_map[person][right]
  end
end

happiness_map = read_happiness_values('input.txt')
guests = happiness_map.keys

max_happiness = guests.permutation.map { |arr| calculate_happiness(arr, happiness_map) }.max
puts max_happiness
