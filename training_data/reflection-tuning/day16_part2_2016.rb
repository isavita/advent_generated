def dragon_curve(s, length)
  while s.length < length
    b = s.reverse.tr('01', '10')
    s = s + '0' + b
  end
  s[0...length]
end

def checksum(s)
  while s.length.even?
    s = s.chars.each_slice(2).map { |a, b| a == b ? '1' : '0' }.join
  end
  s
end

def solve(input, length)
  data = dragon_curve(input, length)
  checksum(data)
end

def solve_large(input, length)
  chunk_size = 1_000_000
  chunks = (length + chunk_size - 1) / chunk_size

  result = ''
  chunks.times do |i|
    start = i * chunk_size
    end_pos = [start + chunk_size, length].min
    chunk_length = end_pos - start

    data = dragon_curve(input, start + chunk_length)[start, chunk_length]
    result += checksum(data)
  end

  checksum(result)
end

input = File.read('input.txt').strip

# Part One
puts solve(input, 272)

# Part Two
puts solve_large(input, 35651584)
