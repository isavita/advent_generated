# Read and parse input
ranges = File.readlines('input.txt').map do |line|
  start, finish = line.strip.split('-').map(&:to_i)
  [start, finish]
end

# Sort ranges by start value
ranges.sort_by!(&:first)

# Merge overlapping ranges
merged = []
ranges.each do |start, finish|
  if merged.empty? || start > merged.last[1] + 1
    merged << [start, finish]
  else
    merged.last[1] = [merged.last[1], finish].max
  end
end

# Part 1: Find the lowest unblocked IP
lowest_unblocked = if merged[0][0] > 0
  0
else
  merged.each_cons(2) do |r1, r2|
    if r1[1] + 1 < r2[0]
      break r1[1] + 1
    end
  end
end

puts "Part 1: #{lowest_unblocked}"

# Part 2: Count allowed IPs
total_blocked = merged.sum { |start, finish| finish - start + 1 }
allowed_ips = 4294967296 - total_blocked

puts "Part 2: #{allowed_ips}"
