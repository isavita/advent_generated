
File.open("./input.txt") do |file|
  earliest_departure = file.gets.not_nil!.to_i64
  bus_ids = file.gets.not_nil!.split(',').map(&.to_s)

  # Part 1
  best_bus = -1_i64
  min_wait = Int64::MAX

  bus_ids.each do |bus_id|
    next if bus_id == "x"
    id = bus_id.to_i64
    wait_time = id - (earliest_departure % id)
    if wait_time < min_wait
      min_wait = wait_time
      best_bus = id
    end
  end

  puts best_bus * min_wait

  # Part 2
  buses = [] of Tuple(Int64, Int64)
  bus_ids.each_with_index do |bus_id, index|
    buses << {bus_id.to_i64, index.to_i64} if bus_id != "x"
  end

  timestamp = 0_i64
  increment = 1_i64

  buses.each do |bus|
    id, offset = bus
    while (timestamp + offset) % id != 0
      timestamp += increment
    end
    increment *= id
  end

  puts timestamp
end
