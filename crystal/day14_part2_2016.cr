
require "digest/md5"

def stretched_md5_hash(input_str)
  hash_result = Digest::MD5.hexdigest(input_str)
  2016.times { hash_result = Digest::MD5.hexdigest(hash_result) }
  hash_result
end

def find_keys(salt, key_index = 64)
  keys_found = 0
  i = 0
  potential_keys = {} of Char => Array(Int32)
  confirmed_keys = [] of Int32

  while keys_found < key_index
    hash_result = stretched_md5_hash("#{salt}#{i}")

    hash_result.chars.uniq.each do |char|
      if hash_result.includes?(char.to_s * 5)
        potential_keys.fetch(char, [] of Int32).each do |potential_index|
          if i - potential_index <= 1000
            confirmed_keys << potential_index
            keys_found += 1
            return confirmed_keys.sort[key_index - 1] if keys_found == key_index
          end
        end
        potential_keys[char] = potential_keys.fetch(char, [] of Int32).select { |index| i - index < 1000 }
      end
    end

    (0..hash_result.size - 3).each do |j|
      if hash_result[j] == hash_result[j + 1] && hash_result[j] == hash_result[j + 2]
        potential_keys[hash_result[j]] ||= [] of Int32
        potential_keys[hash_result[j]] << i
        break
      end
    end
    i += 1
  end
  
end

def main
  salt = File.read("input.txt").strip
  index_of_64th_key = find_keys(salt)
  puts index_of_64th_key
end

main
