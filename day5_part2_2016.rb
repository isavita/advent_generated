
require 'digest'

door_id = File.read('input.txt').strip
password1 = ''
password2 = Array.new(8)
index = 0

while password1.length < 8 || password2.any?(&:nil?)
  hash = Digest::MD5.hexdigest("#{door_id}#{index}")
  if hash.start_with?('00000')
    password1 << hash[5] if password1.length < 8
    pos = hash[5].to_i(16)
    password2[pos] = hash[6] if pos < 8 && password2[pos].nil?
  end
  index += 1
end

puts password1
puts password2.join
