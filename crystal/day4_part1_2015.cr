
require "digest/md5"
require "file"

secret_key = File.read("input.txt").chomp
number = 0

loop do
  hash = Digest::MD5.hexdigest("#{secret_key}#{number}")
  if hash.starts_with?("00000")
    puts number
    break
  end
  number += 1
end
