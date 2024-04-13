class RangeMap
  attr_accessor :src_start, :dest_start, :length

  def initialize(src_start, dest_start, length)
    @src_start = src_start
    @dest_start = dest_start
    @length = length
  end

  def reverse_convert_number(number)
    return @src_start + (number - @dest_start) if number >= @dest_start && number < @dest_start + @length
    nil
  end
end

def is_in_seed_ranges(number, seed_ranges)
  seed_ranges.any? { |r| number >= r[0] && number < r[0] + r[1] }
end

seed_ranges = []
maps = []
current_ranges = []

File.foreach('input.txt') do |line|
  line.chomp!
  if line.include?('map:')
    unless current_ranges.empty?
      maps << current_ranges
      current_ranges = []
    end
  elsif line.start_with?('seeds:')
    seed_strs = line[7..].split
    seed_strs.each_slice(2) do |start, length|
      seed_ranges << [start.to_i, length.to_i]
    end
  else
    numbers = line.split
    if numbers.size == 3
      src_start = numbers[1].to_i
      dest_start = numbers[0].to_i
      length = numbers[2].to_i
      current_ranges << RangeMap.new(src_start, dest_start, length)
    end
  end
end

maps << current_ranges unless current_ranges.empty?

location = 0
loop do
  seed = location
  maps.reverse_each do |map|
    new_seed = nil
    map.each do |range_map|
      new_seed = range_map.reverse_convert_number(seed)
      break if new_seed
    end
    seed = new_seed if new_seed
  end

  if is_in_seed_ranges(seed, seed_ranges)
    puts location
    break
  end

  location += 1
end