def from_snafu(s)
  n = 0
  s.each_char do |c|
    n *= 5
    case c
    when '='
      n -= 2
    when '-'
      n -= 1
    else
      n += c.to_i
    end
  end
  n
end

def to_snafu(n)
  b = []
  while n > 0
    case n % 5
    when 3
      n += 5
      b << '='
    when 4
      n += 5
      b << '-'
    else
      b << (n % 5).to_s
    end
    n /= 5
  end
  b.reverse.join
end

sum = 0
File.readlines('input.txt').each do |line|
  sum += from_snafu(line.chomp)
end
puts to_snafu(sum)