require 'json'

def compare(left, right)
  if left.is_a?(Integer) && right.is_a?(Integer)
    return -1 if left < right
    return 1 if left > right
    return 0
  elsif left.is_a?(Array) && right.is_a?(Array)
    i = 0
    while i < [left.length, right.length].min
      result = compare(left[i], right[i])
      return result unless result == 0
      i += 1
    end
    return -1 if left.length < right.length
    return 1 if left.length > right.length
    return 0
  else
    return compare([left].flatten, [right].flatten)
  end
end

pairs = []
current_pair = []
File.readlines('input.txt').each do |line|
  if line.strip.empty?
    pairs << current_pair
    current_pair = []
  else
    current_pair << JSON.parse(line)
  end
end
pairs << current_pair

result = 0
pairs.each_with_index do |pair, i|
  if compare(pair[0], pair[1]) == -1
    result += i + 1
  end
end

puts result