
#!/usr/bin/env crystal
def contains(ranges : Array(Tuple(Int64, Int64)), x : Int64) : Bool
  l = 0
  r = ranges.size
  while l < r
    m = (l + r) >> 1
    if x < ranges[m][0]
      r = m
    elsif x > ranges[m][1]
      l = m + 1
    else
      return true
    end
  end
  false
end

ranges = [] of Tuple(Int64, Int64)
ids    = [] of Int64
parsing = true

File.each_line("input.txt") do |line|
  s = line.strip
  if s.empty?
    parsing = false if parsing
  elsif parsing
    a, b = s.split('-')
    ranges << {a.to_i64, b.to_i64}
  else
    ids << s.to_i64
  end
end

merged = begin
  sorted = ranges.sort_by { |t| t[0] }
  buf = [] of Tuple(Int64, Int64)
  sorted.each do |mn, mx|
    if buf.empty? || mn > buf.last[1]
      buf << {mn, mx}
    elsif mx > buf.last[1]
      buf[buf.size - 1] = {buf.last[0], mx}
    end
  end
  buf
end

fresh = ids.count { |id| contains(merged, id) }
puts "Number of fresh ingredients: #{fresh}"
