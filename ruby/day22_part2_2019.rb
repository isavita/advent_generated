
def egcd(a, b)
  return [b, 0, 1] if a == 0
  gcd, y, x = egcd(b % a, a)
  [gcd, x - (b / a) * y, y]
end

def modinv(a, m)
  gcd, x, _ = egcd(a, m)
  raise "Modular inverse does not exist" if gcd != 1
  x = x % m
  x < 0 ? x + m : x
end

SIZE = 119315717514047
ITERATIONS = 101741582076661

def solve
  offset = 0
  increment = 1

  File.readlines('input.txt').each do |line|
    case line.strip
    when 'deal into new stack'
      increment *= -1
      offset += increment
    when /^cut (-?\d+)/
      n = $1.to_i
      offset += n * increment
    when /^deal with increment (\d+)/
      n = $1.to_i
      increment *= modinv(n, SIZE)
    end
  end

  final_incr = increment.pow(ITERATIONS, SIZE)
  
  final_offs = (1 - final_incr).* modinv(1 - increment, SIZE)
  final_offs *= offset

  answer = (2020 * final_incr + final_offs) % SIZE
  puts answer
end

solve
