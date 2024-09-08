DECK_SIZE = 119315717514047
SHUFFLE_COUNT = 101741582076661
TARGET_POSITION = 2020

def mod_pow(base, exp, mod)
  result = 1
  base = base % mod
  while exp > 0
    result = (result * base) % mod if exp.odd?
    exp >>= 1
    base = (base * base) % mod
  end
  result
end

def mod_inverse(a, m)
  mod_pow(a, m - 2, m)
end

a, b = 1, 0
File.readlines('input.txt').each do |line|
  case line.strip
  when 'deal into new stack'
    a = -a % DECK_SIZE
    b = (DECK_SIZE - 1 - b) % DECK_SIZE
  when /cut (-?\d+)/
    n = $1.to_i
    b = (b - n) % DECK_SIZE
  when /deal with increment (\d+)/
    n = $1.to_i
    a = (a * n) % DECK_SIZE
    b = (b * n) % DECK_SIZE
  end
end

ma = mod_pow(a, SHUFFLE_COUNT, DECK_SIZE)
mb = (b * (ma - 1) * mod_inverse(a - 1, DECK_SIZE)) % DECK_SIZE

result = ((TARGET_POSITION - mb) * mod_inverse(ma, DECK_SIZE)) % DECK_SIZE
puts result
