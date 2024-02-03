values = []
File.open("input.txt").each do |line|
  values << line.chomp
end

def filter_values(values, criteria)
  values[0].length.times do |i|
    zeros, ones = 0, 0
    values.each do |val|
      val[i] == '0' ? zeros += 1 : ones += 1
    end
    keep = criteria.call(zeros, ones)
    values = filter_by_bit(values, i, keep)
    break if values.length == 1
  end
  values[0]
end

def filter_by_bit(values, bit_index, keep)
  filtered = []
  values.each do |val|
    filtered << val if val[bit_index] == keep
  end
  filtered
end

oxygen_generator_rating = filter_values(values, ->(zeros, ones) { zeros > ones ? '0' : '1' })
oxygen_generator_rating_int = oxygen_generator_rating.to_i(2)

co2_scrubber_rating = filter_values(values, ->(zeros, ones) { zeros <= ones ? '0' : '1' })
co2_scrubber_rating_int = co2_scrubber_rating.to_i(2)

puts oxygen_generator_rating_int * co2_scrubber_rating_int