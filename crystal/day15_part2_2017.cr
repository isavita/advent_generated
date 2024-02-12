
file = File.open("input.txt")
gen_a_start = file.gets.try(&.to_i64)
gen_b_start = file.gets.try(&.to_i64)
file.close

if gen_a_start.nil? || gen_b_start.nil?
  puts "Error: Unable to read initial values from the file."
  exit
end

gen_a_factor = 16807_i64
gen_b_factor = 48271_i64
modulus = 2147483647_i64

gen_a = gen_a_start
gen_b = gen_b_start
matches = 0

(0..4_999_999).each do |i|
  # Generate next value for A that is a multiple of 4
  loop do
    gen_a = (gen_a * gen_a_factor) % modulus
    break if gen_a % 4 == 0
  end

  # Generate next value for B that is a multiple of 8
  loop do
    gen_b = (gen_b * gen_b_factor) % modulus
    break if gen_b % 8 == 0
  end

  matches += 1 if gen_a & 0xFFFF == gen_b & 0xFFFF
end

puts matches
