def merge_ranges(ranges)
  ranges.sort_by!(&:begin)
  merged = [ranges[0]]
  ranges[1..-1].each do |range|
    last = merged[-1]
    if range.begin <= last.end + 1
      merged[-1] = (last.begin..[last.end, range.end].max)
    else
      merged << range
    end
  end
  merged
end

def find_lowest_unblocked_ip(blocked_ranges)
  merged = merge_ranges(blocked_ranges)
  return 0 if merged[0].begin > 0
  
  lowest_unblocked = 0
  merged.each do |range|
    return lowest_unblocked if lowest_unblocked < range.begin
    lowest_unblocked = [lowest_unblocked, range.end + 1].max
  end
  lowest_unblocked
end

blocked_ranges = File.readlines('input.txt').map do |line|
  start, end_ = line.strip.split('-').map(&:to_i)
  (start..end_)
end

puts find_lowest_unblocked_ip(blocked_ranges)
