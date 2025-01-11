
record RangeMap, src_start : Int64, dest_start : Int64, length : Int64

def reverse_convert_number(number, ranges)
  ranges.each do |r|
    if number >= r.dest_start && number < r.dest_start + r.length
      return r.src_start + (number - r.dest_start)
    end
  end
  number
end

def in_seed_ranges?(number, ranges)
  ranges.each do |r|
    if number >= r[0] && number < r[0] + r[1]
      return true
    end
  end
  false
end

file = File.open("input.txt")
seed_ranges = [] of Tuple(Int64, Int64)
current_ranges = [] of RangeMap
maps = [] of Array(RangeMap)

file.each_line do |line|
  if line.includes?("map:")
    if current_ranges.size > 0
      maps << current_ranges
      current_ranges = [] of RangeMap
    end
  elsif line.starts_with?("seeds:")
    seed_strs = line[7..-1].split(" ")
    seed_strs.each_slice(2) do |slice|
      start = slice[0].to_i64
      length = slice[1].to_i64
      seed_ranges << {start, length}
    end
  else
    numbers = line.split(" ")
    if numbers.size == 3
      src_start = numbers[1].to_i64
      dest_start = numbers[0].to_i64
      length = numbers[2].to_i64
      current_ranges << RangeMap.new(src_start, dest_start, length)
    end
  end
end
if current_ranges.size > 0
  maps << current_ranges
end

location = 0
loop do
  seed = location
  maps.reverse_each do |map|
    seed = reverse_convert_number(seed, map)
  end

  if in_seed_ranges?(seed, seed_ranges)
    puts location
    break
  end
  location += 1
end
