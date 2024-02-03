
value_dict = {'J' => 1, '2' => 2, '3' => 3, '4' => 4, '5' => 5, '6' => 6, '7' => 7, '8' => 8, '9' => 9, 'T' => 10, 'Q' => 11, 'K' => 12, 'A' => 13}

hands = []

File.open("input.txt").each do |line|
  next if line.strip.empty?

  cards = line.scan(/[\dAKQJT]+/).first
  bid = line.scan(/ [\d]+/).first[1..-1].to_i

  hands << { cards: cards, bid: bid }
end

matches = [[], [], [], [], [], [], []]

hands.each do |hand|
  count = Hash.new(0)

  hand[:cards].each_char { |i| count[i] += 1 }

  if count['J'] > 0
    high_v = 0
    high_key = 'J'
    count.each do |key, value|
      if key != 'J'
        if value > high_v
          high_key = key
          high_v = value
        elsif value == high_v && value_dict[key] > value_dict[high_key]
          high_key = key
        end
      end
    end
    if high_key != 'J'
      count[high_key] += count['J']
      count.delete('J')
    end
  end

  value = count.values.reduce(:*)

  case value
  when 1
    matches[6] << hand
  when 2
    matches[5] << hand
  when 3
    matches[3] << hand
  when 4
    if count.keys.length == 2
      matches[1] << hand
    else
      matches[4] << hand
    end
  when 5
    matches[0] << hand
  when 6
    matches[2] << hand
  else
    puts "oh no"
  end
end

converted_matches = []

matches.each do |x|
  temp = []
  x.each do |i|
    y = i[:cards].gsub('A', 'E').gsub('T', 'A').gsub('J', '1').gsub('Q', 'C').gsub('K', 'D')
    val = y.to_i(16)
    temp << [val, i[:bid]]
  end
  temp.sort! { |a, b| b[0] <=> a[0] }
  temp.each { |i| converted_matches << i }
end

total = 0
converted_matches.each_with_index do |val, index|
  total += val[1] * (converted_matches.length - index)
end

puts total
