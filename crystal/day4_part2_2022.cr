def parse_range(s : String) : Array(Int32)
  split = s.split("-")
  start = split[0].to_i
  ends = split[1].to_i
  return [start, ends]
end

count = 0
File.open("input.txt") do |file|
  file.each_line do |line|
    pair = line.strip.split(",")

    left = parse_range(pair[0])
    right = parse_range(pair[1])

    if left[0] <= right[1] && left[1] >= right[0]
      count += 1
    end
  end
end

puts count