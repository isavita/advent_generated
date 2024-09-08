require 'digest'

def find_advent_coin(secret_key, leading_zeroes)
  target = "0" * leading_zeroes
  number = 1
  md5 = Digest::MD5.new

  loop do
    hash = md5.hexdigest(secret_key + number.to_s)
    return number if hash.start_with?(target)
    number += 1
  end
end

# Read input from file
secret_key = File.read('input.txt').strip

# Part One
part_one_result = find_advent_coin(secret_key, 5)
puts "Part One: #{part_one_result}"

# Part Two
part_two_result = find_advent_coin(secret_key, 6)
puts "Part Two: #{part_two_result}"
