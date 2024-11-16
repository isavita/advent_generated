
def firewall(input : String) : Int32
  blocked_ranges = input.lines.map do |line|
    range = line.split('-').map(&.to_i)
    {range[0], range[1]}
  end.sort_by! { |r| [r[0], r[1]] }

  merged = [] of Tuple(Int32, Int32)
  blocked_ranges.each do |r|
    if merged.empty? || merged.last[1] < r[0] - 1
      merged << r
    else
      last = merged.pop
      merged << {last[0], Math.max(last[1], r[1])}
    end
  end

  merged << {UInt32::MAX.to_i, 0} if merged.last[1] != UInt32::MAX

  total_allowed = 0
  (1...merged.size).each do |i|
    total_allowed += merged[i][0] - merged[i-1][1] - 1
  end

  total_allowed
end

def read_file(path : String) : String
  File.read(path).chomp
end

input = read_file("input.txt")
puts firewall(input)
