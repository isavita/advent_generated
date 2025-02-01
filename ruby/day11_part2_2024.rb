
def trim_leading_zeros(s)
  s.sub(/^(0+)(?=\d)/, '')
end

def split_stone(s)
  mid = s.length / 2
  left = trim_leading_zeros(s[0...mid])
  right = trim_leading_zeros(s[mid..-1])
  left = '0' if left.empty?
  right = '0' if right.empty?
  [left, right]
end

def multiply_by_2024(s)
  num = s.chars.map(&:to_i)
  multiplier = [2, 0, 2, 4]
  result = [0] * (num.length + multiplier.length)

  num.each_index.reverse_each do |i|
    carry = 0
    multiplier.each_index.reverse_each do |j|
      product = num[i] * multiplier[j] + result[i + j + 1] + carry
      result[i + j + 1] = product % 10
      carry = product / 10
    end
    result[i] += carry
  end

  result.join.sub(/^(0+)(?=\d)/, '')
end

file = File.open('input.txt')
line = file.readline.chomp
stones_str = line.split

stones_map = Hash.new(0)
stones_str.each { |s| stones_map[s] += 1 }

75.times do
  new_stones_map = Hash.new(0)
  stones_map.each do |stone, count|
    if stone == '0'
      new_stones_map['1'] += count
    elsif stone.length.even?
      left, right = split_stone(stone)
      new_stones_map[left] += count
      new_stones_map[right] += count
    else
      new_stone = multiply_by_2024(stone)
      new_stones_map[new_stone] += count
    end
  end
  stones_map = new_stones_map
end

total_stones = stones_map.values.sum
puts total_stones
