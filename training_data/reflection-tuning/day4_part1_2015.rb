require 'digest'

def find_advent_coin(secret_key)
  number = 1
  loop do
    hash = Digest::MD5.hexdigest("#{secret_key}#{number}")
    return number if hash.start_with?('00000')
    number += 1
  end
end

secret_key = File.read('input.txt').strip
result = find_advent_coin(secret_key)
puts result
