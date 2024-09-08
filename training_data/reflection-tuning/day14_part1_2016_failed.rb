require 'digest'

def generate_hash(salt, index)
  Digest::MD5.hexdigest(salt + index.to_s)
end

def find_triple(hash)
  hash.chars.each_cons(3) do |a, b, c|
    return a if a == b && b == c
  end
  nil
end

def has_quintuple?(hash, char)
  hash.include?(char * 5)
end

def find_64th_key(salt)
  keys_found = 0
  index = 0
  hash_cache = {}

  while keys_found < 64
    hash = hash_cache[index] ||= generate_hash(salt, index)
    if triple = find_triple(hash)
      if (index+1..index+1000).any? { |i| has_quintuple?(hash_cache[i] ||= generate_hash(salt, i), triple) }
        keys_found += 1
        puts "Found key #{keys_found} at index #{index}" if keys_found == 64
      end
    end
    index += 1
  end

  index - 1  # Return the last index that produced a key
end

salt = "abc"  # Replace with your actual salt
result = find_64th_key(salt)
puts result
