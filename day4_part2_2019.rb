
input = File.read('input.txt').split('-').map(&:to_i)

def valid_passwords_count(range, strict_double)
  count = 0
  (range[0]..range[1]).each do |num|
    digits = num.digits.reverse
    next if digits != digits.sort
    if strict_double
      count += 1 if digits.chunk_while { |a, b| a == b }.any? { |chunk| chunk.size == 2 }
    else
      count += 1 if digits.chunk_while { |a, b| a == b }.any? { |chunk| chunk.size >= 2 }
    end
  end
  count
end

puts valid_passwords_count(input, false)
puts valid_passwords_count(input, true)
