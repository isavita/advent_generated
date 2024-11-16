
require "digest/md5"

def find_password(door_id)
  password = Array(Char).new(8, '?')
  filled_positions = 0
  found = Array(Bool).new(8, false)

  i = 0
  while filled_positions < 8
    hash = Digest::MD5.hexdigest("#{door_id}#{i}")
    if hash.starts_with?("00000")
      pos = hash[5]
      if pos >= '0' && pos <= '7'
        pos_index = pos - '0'
        if !found[pos_index]
          found[pos_index] = true
          password[pos_index] = hash[6]
          filled_positions += 1
        end
      end
    end
    i += 1
  end

  password.join
end

door_id = File.read("input.txt").strip
puts find_password(door_id)
