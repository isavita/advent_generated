require 'digest'

input = File.read('input.txt').strip

number = 1
while true
  hash = Digest::MD5.hexdigest("#{input}#{number}")
  break if hash.start_with?('000000')
  number += 1
end

puts number