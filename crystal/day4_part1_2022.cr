
def parse_range(r : String) : Tuple(Int32, Int32)
  start, finish = r.split("-").map(&.to_i)
  {start, finish}
end

count = 0
File.open("input.txt") do |file|
  file.each_line do |line|
    ranges = line.strip.split(",")
    next if ranges.size != 2

    start1, end1 = parse_range(ranges[0])
    start2, end2 = parse_range(ranges[1])

    if (start1 <= start2 && end1 >= end2) || (start2 <= start1 && end2 >= end1)
      count += 1
    end
  end
end

puts count
