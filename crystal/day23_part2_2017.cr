
def is_prime(n : Int32) : Bool
  (2..Math.sqrt(n).to_i).each do |i|
    return false if n % i == 0
  end
  true
end

b = 57 * 100 + 100000
c = b + 17000
h = 0

(b..c).step(17) do |x|
  h += 1 unless is_prime(x)
end

puts h
