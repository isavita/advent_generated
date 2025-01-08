
require "digest/md5"

door_id = File.read("input.txt").strip
password = ""
i = 0
while password.size < 8
  hash = Digest::MD5.hexdigest(door_id + i.to_s)
  if hash.starts_with?("00000")
    password += hash[5]
  end
  i += 1
end
puts password
