
require 'digest'

door_id = File.read('input.txt').strip

password = ''
index = 0

while password.length < 8
  hash = Digest::MD5.hexdigest("#{door_id}#{index}")
  if hash.start_with?('00000')
    password += hash[5]
  end
  index += 1
end

puts password
