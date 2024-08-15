require "file_utils"

def filter_values(values, &criteria : (Int32, Int32) -> Char)
  values.size.times do |i|
    zeros, ones = 0, 0
    values.each do |val|
      zeros += 1 if val[i] == '0'
      ones += 1 if val[i] == '1'
    end
    keep = criteria.call(zeros, ones)
    values = filter_by_bit(values, i, keep)
    break if values.size == 1
  end
  values.first
end

def filter_by_bit(values, bit_index, keep)
  values.select { |val| val[bit_index] == keep }
end

File.open("input.txt") do |file|
  values = file.each_line.to_a
  oxygen_generator_rating = filter_values(values) do |zeros, ones|
    zeros > ones ? '0' : '1'
  end
  oxygen_generator_rating_int = oxygen_generator_rating.to_i(2)

  co2_scrubber_rating = filter_values(values) do |zeros, ones|
    zeros <= ones ? '0' : '1'
  end
  co2_scrubber_rating_int = co2_scrubber_rating.to_i(2)

  puts oxygen_generator_rating_int * co2_scrubber_rating_int
end