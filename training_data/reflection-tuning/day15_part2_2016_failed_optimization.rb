def extended_gcd(a, b)
  last_remainder, remainder = a.abs, b.abs
  x, last_x, y, last_y = 0, 1, 1, 0
  while remainder != 0
    last_remainder, (quotient, remainder) = remainder, last_remainder.divmod(remainder)
    x, last_x = last_x - quotient*x, x
    y, last_y = last_y - quotient*y, y
  end
  return last_remainder, last_x * (a < 0 ? -1 : 1)
end

def mod_inv(e, et)
  g, x = extended_gcd(e, et)
  raise 'Multiplicative inverse does not exist' if g != 1
  x % et
end

def chinese_remainder(mods, remainders)
  total = 0
  product = mods.inject(:*)
  
  mods.zip(remainders).each do |mi, ri|
    pi = product / mi
    total += ri * mod_inv(pi, mi) * pi
  end
  
  total % product
end

def solve_disc_puzzle(discs)
  mods = []
  remainders = []
  
  discs.each_with_index do |(positions, initial_pos), i|
    mods << positions
    remainders << (positions - initial_pos - (i + 1)) % positions
  end
  
  chinese_remainder(mods, remainders)
end

# Part 1
discs = [
  [13, 1],
  [19, 10],
  [3, 2],
  [7, 1],
  [5, 3],
  [17, 5]
]
puts "Part 1: #{solve_disc_puzzle(discs)}"

# Part 2
discs << [11, 0]
puts "Part 2: #{solve_disc_puzzle(discs)}"
