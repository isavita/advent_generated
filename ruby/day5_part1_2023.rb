
class RangeMap
  attr_accessor :src_start, :dest_start, :length

  def initialize(src_start, dest_start, length)
    @src_start = src_start
    @dest_start = dest_start
    @length = length
  end
end

def convert_number(number, ranges)
  ranges.each do |r|
    if number >= r.src_start && number < r.src_start + r.length
      return r.dest_start + (number - r.src_start)
    end
  end
  number
end

seeds = []
maps = []
current_ranges = []

File.open("input.txt").each do |line|
  if line.include?("map:")
    maps << current_ranges unless current_ranges.empty?
    current_ranges = []
  elsif line.start_with?("seeds:")
    seed_strs = line[7..-1].split(" ")
    seed_strs.each { |s| seeds << s.to_i }
  else
    numbers = line.split
    if numbers.length == 3
      src_start = numbers[1].to_i
      dest_start = numbers[0].to_i
      length = numbers[2].to_i
      current_ranges << RangeMap.new(src_start, dest_start, length)
    end
  end
end
maps << current_ranges

min_location = -1
seeds.each do |seed|
  location = seed
  maps.each do |m|
    location = convert_number(location, m)
  end

  min_location = location if min_location == -1 || location < min_location
end

puts min_location
