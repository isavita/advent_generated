def nice_string?(str)
  # Rule 1: Pair appears twice without overlapping
  pairs = {}
  (0...str.length-1).each do |i|
    pair = str[i,2]
    if pairs[pair] && i - pairs[pair] > 1
      return true if str.match?(/(..).*\1/)
    end
    pairs[pair] ||= i
  end

  # Rule 2: Letter repeats with one letter between
  (0...str.length-2).any? { |i| str[i] == str[i+2] }
end

nice_count = File.readlines('input.txt').count do |line|
  nice_string?(line.strip)
end

puts nice_count
