
require 'digest'

secret_key = File.read('input.txt').strip
number = 0

loop do
  hash = Digest::MD5.hexdigest(secret_key + number.to_s)
  break if hash.start_with?('00000')
  number += 1
end

puts number
