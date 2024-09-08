require 'digest'

def find_64th_key(salt, stretch = false)
  keys = []
  index = 0
  hash_cache = {}
  quintuplets = Array.new(1000) { Set.new }

  while keys.size < 64
    hash = stretched_hash(salt, index, stretch, hash_cache)
    
    triplet = hash.scan(/(.)\1\1/).first
    if triplet
      char = triplet[0]
      if quintuplets.any? { |set| set.include?(char) }
        keys << index
      end
    end

    quint = hash.scan(/(.)\1\1\1\1/).first
    if quint
      quintuplets[index % 1000].add(quint[0])
    end

    quintuplets[index % 1000].clear if index >= 1000
    index += 1
  end

  keys.last
end

def stretched_hash(salt, index, stretch, cache)
  key = "#{salt}#{index}"
  return cache[key] if cache.key?(key)

  hash = Digest::MD5.hexdigest(key)
  if stretch
    2016.times { hash = Digest::MD5.hexdigest(hash) }
  end
  cache[key] = hash
end

salt = "abc"  # Replace with your actual salt
puts "Part 1: #{find_64th_key(salt)}"
puts "Part 2: #{find_64th_key(salt, true)}"
