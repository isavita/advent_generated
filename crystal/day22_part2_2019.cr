
SIZE = 119315717514047_i64

def modinv(a, m)
  m0 = m
  x0 = 0_i128
  x1 = 1_i128
  return 0_i128 if m == 1
  while a > 1
    q = a // m
    t = m
    m = a % m
    a = t
    t = x0
    x0 = x1 - q * x0
    x1 = t
  end
  x1 += m0 if x1 < 0
  x1
end

size = SIZE.to_i128
iter = 101741582076661_i128
offset = 0_i128
increment = 1_i128

File.open("input.txt") do |f|
  f.each_line do |line|
    line = line.strip
    if line == "deal into new stack"
      increment = -increment
      offset += increment
    elsif line.starts_with?("cut")
      n = line.split[1].to_i64.to_i128
      offset += n * increment
    elsif line.starts_with?("deal with increment")
      n = line.split[-1].to_i64.to_i128
      increment = (increment * modpow(n, size - 2, size)) % size
    end
  end
end

final_incr = modpow(increment, iter, size)
final_offs = (1_i128 - final_incr) % size
inv_mod = modpow((1_i128 - increment) % size, size - 2, size)
final_offs = (final_offs * inv_mod % size) * offset % size

answer = (2020_i128 * final_incr + final_offs) % size
puts answer

def modpow(base, exp, mod)
  result = 1_i128
  base = base % mod
  while exp > 0
    result = (result * base) % mod if exp.odd?
    base = (base * base) % mod
    exp >>= 1
  end
  result
end
