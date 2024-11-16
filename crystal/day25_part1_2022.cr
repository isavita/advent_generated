
def from_snafu(s : String) : Int64
  n = 0_i64
  s.each_char do |c|
    n *= 5
    case c
    when '='
      n -= 2
    when '-'
      n -= 1
    else
      n += c.to_i64
    end
  end
  n
end

def to_snafu(n : Int64) : String
  b = [] of Char
  while n > 0
    case n % 5
    when 3
      n += 5
      b << '='
    when 4
      n += 5
      b << '-'
    else
      b << (n % 5).to_s[0]
    end
    n //= 5
  end
  b.reverse.join
end

def main
  sum = 0_i64
  File.each_line("input.txt") do |line|
    sum += from_snafu(line.chomp)
  end
  puts to_snafu(sum)
end

main
