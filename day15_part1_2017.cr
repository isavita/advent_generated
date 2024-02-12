
file = File.open("input.txt")

gen_a_start = file.gets.try(&.to_i64) || 0
gen_b_start = file.gets.try(&.to_i64) || 0

gen_a_factor = 16807_i64
gen_b_factor = 48271_i64
modulus = 2147483647_i64

gen_a = gen_a_start
gen_b = gen_b_start
matches = 0

(1..40_000_000).each do
  gen_a = (gen_a * gen_a_factor) % modulus
  gen_b = (gen_b * gen_b_factor) % modulus

  matches += 1 if gen_a & 0xFFFF == gen_b & 0xFFFF
end

puts matches
