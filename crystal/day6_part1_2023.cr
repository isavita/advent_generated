def calculate_ways_to_win(time, record)
  ways_to_win = 0
  (1...time).each do |hold_time|
    travel_time = time - hold_time
    distance = hold_time * travel_time
    ways_to_win += 1 if distance > record
  end
  ways_to_win
end

times = [] of Int32
distances = [] of Int32

File.open("input.txt", "r") do |file|
  file.each_line.with_index do |line, index|
    next if line.strip.empty? # Skip empty lines
    line.split(" ").each do |value|
      # Only attempt to convert valid integers
      if value =~ /^\d+$/
        (index == 0 ? times : distances) << value.to_i32
      end
    end
  end
end

total_ways = 1
times.each_with_index do |time, i|
  ways = calculate_ways_to_win(time, distances[i])
  total_ways *= ways
end

puts total_ways