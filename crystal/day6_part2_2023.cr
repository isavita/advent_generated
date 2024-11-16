
def count_ways_to_win(time : Int64, distance : Int64) : Int64
  ways = 0_i64
  (1_i64...time).each do |hold_time|
    travel_time = time - hold_time
    traveled_distance = hold_time * travel_time
    ways += 1 if traveled_distance > distance
  end
  ways
end

def solve_part1(times : Array(Int64), distances : Array(Int64)) : Int64
  times.zip(distances).map { |time, distance| count_ways_to_win(time, distance) }.product
end

def solve_part2(times : Array(Int64), distances : Array(Int64)) : Int64
  big_time = times.map(&.to_s).join.to_i64
  big_distance = distances.map(&.to_s).join.to_i64
  count_ways_to_win(big_time, big_distance)
end

# Read input from file
input_lines = File.read_lines("input.txt")

# Parse input
times = input_lines[0].split(':')[1].split.map(&.to_i64)
distances = input_lines[1].split(':')[1].split.map(&.to_i64)

# Solve and print results
puts "Part 1: #{solve_part1(times, distances)}"
puts "Part 2: #{solve_part2(times, distances)}"
