def earliest_bus(timestamp, bus_ids)
  min_wait = Float64::INFINITY
  best_bus = 0

  bus_ids.each do |bus_id|
    wait = ((timestamp / bus_id.to_f).ceil * bus_id) - timestamp
    if wait < min_wait
      min_wait = wait
      best_bus = bus_id
    end
  end

  best_bus * min_wait
end

timestamp, bus_ids = File.read("input.txt").split("\n")
timestamp = timestamp.to_i
bus_ids = bus_ids.split(",").reject { |id| id == "x" }.map { |id| id.to_i }

puts earliest_bus(timestamp, bus_ids)