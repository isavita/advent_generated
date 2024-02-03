
def calculate_ways_to_win_long_race(time, record)
  ways_to_win = 0
  (1...time).each do |hold_time|
    travel_time = time - hold_time
    distance = hold_time * travel_time
    ways_to_win += 1 if distance > record
  end
  ways_to_win
end

file = File.open("input.txt")
time, distance = 0, 0

file.each_line do |line|
  next if line.strip.empty?
  parts = line.split(":")
  value = parts[1].delete(" ").to_i
  time == 0 ? time = value : distance = value
end

ways_to_win = calculate_ways_to_win_long_race(time, distance)

puts ways_to_win
