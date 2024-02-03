
def dragon_curve(a)
  b = a.reverse.tr('01', '10')
  "#{a}0#{b}"
end

def checksum(data)
  sum = data.chars.each_slice(2).map { |x, y| x == y ? '1' : '0' }.join
  sum.length.even? ? checksum(sum) : sum
end

def fill_disk(initial_state, length)
  data = initial_state
  data = dragon_curve(data) while data.length < length
  data[0...length]
end

def solve(input, length)
  data = fill_disk(input, length)
  checksum(data)
end

input = File.read('input.txt').strip
puts solve(input, 272)
puts solve(input, 35651584)
