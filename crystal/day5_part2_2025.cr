
ranges = [] of {Int64, Int64}

File.each_line("input.txt") do |line|
  line = line.strip
  break if line.empty?
  a_str, b_str = line.split('-', 2)
  a = a_str.strip.to_i64
  b = b_str.strip.to_i64
  min, max = a <= b ? {a, b} : {b, a}
  ranges << {min, max}
end

if ranges.empty?
  puts "Total fresh IDs: 0"
  exit
end

ranges.sort_by! { |r| {r[0], r[1]} }

total = 0_i64
cur_min, cur_max = ranges[0]

ranges[1..-1].each do |r|
  if r[0] <= cur_max
    cur_max = r[1] if r[1] > cur_max
  else
    total += cur_max - cur_min + 1
    cur_min, cur_max = r
  end
end

total += cur_max - cur_min + 1
puts "Total fresh IDs: #{total}"
