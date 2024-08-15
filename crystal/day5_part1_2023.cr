class RangeMap
  property src_start : Int64
  property dest_start : Int64
  property length : Int64

  def initialize(src_start : Int64, dest_start : Int64, length : Int64)
    @src_start = src_start
    @dest_start = dest_start
    @length = length
  end
end

def convert_number(number : Int64, ranges : Array(RangeMap)) : Int64
  ranges.each do |r|
    if number >= r.src_start && number < r.src_start + r.length
      return r.dest_start + (number - r.src_start)
    end
  end
  number
end

def main
  file = File.open("input.txt")
  seeds = [] of Int64
  current_ranges = [] of RangeMap
  maps = [] of Array(RangeMap)

  file.each_line do |line|
    if line.includes?("map:")
      maps << current_ranges unless current_ranges.empty?
      current_ranges = [] of RangeMap
    elsif line.starts_with?("seeds:")
      line[7..-1].split(" ").each do |s|
        seeds << s.to_i64
      end
    else
      numbers = line.split.map(&.to_i64)
      if numbers.size == 3
        dest_start, src_start, length = numbers
        current_ranges << RangeMap.new(src_start, dest_start, length)
      end
    end
  end
  maps << current_ranges unless current_ranges.empty?

  min_location = -1
  seeds.each do |seed|
    location = seed
    maps.each do |m|
      location = convert_number(location, m)
    end
    min_location = location if min_location == -1 || location < min_location
  end

  puts min_location
end

main