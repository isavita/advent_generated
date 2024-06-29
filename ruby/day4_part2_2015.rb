
require 'digest'

input = File.read('input.txt').strip
number = 0

loop do
  hash = Digest::MD5.hexdigest(input + number.to_s)
  break if hash.start_with?('000000')
  number += 1
end

puts number
