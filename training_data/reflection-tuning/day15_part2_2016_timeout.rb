def parse_input(filename)
  File.readlines(filename).map do |line|
    parts = line.scan(/\d+/).map(&:to_i)
    [parts[1], parts[3]]  # [positions, start_position]
  end
end

def passes_through?(discs, start_time)
  discs.each_with_index.all? do |(positions, start_position), index|
    (start_time + index + 1 + start_position) % positions == 0
  end
end

def find_start_time(discs)
  time = 0
  step = discs.map(&:first).reduce(1, :lcm)
  time += step until passes_through?(discs, time)
  time
end

discs = parse_input('input.txt')

# Part One
puts find_start_time(discs)

# Part Two
discs << [11, 0]
puts find_start_time(discs)
