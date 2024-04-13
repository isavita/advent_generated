require 'digest'

def main
  salt = File.read('input.txt').strip
  hash_cache = {}
  keys = 0
  index = 0

  while keys < 64
    hash = get_stretched_md5_hash(salt + index.to_s, hash_cache)
    triplet = find_triplet(hash)
    if triplet
      1.upto(1000) do |i|
        next_hash = get_stretched_md5_hash(salt + (index + i).to_s, hash_cache)
        if next_hash.include?(triplet * 5)
          keys += 1
          break
        end
      end
    end
    index += 1
  end

  puts index - 1
end

def get_stretched_md5_hash(input, hash_cache)
  return hash_cache[input] if hash_cache.key?(input)

  hash = get_md5_hash(input)
  2016.times { hash = get_md5_hash(hash) }
  hash_cache[input] = hash
  hash
end

def get_md5_hash(input)
  Digest::MD5.hexdigest(input)
end

def find_triplet(hash)
  (0..hash.length - 3).each do |i|
    return hash[i] if hash[i] == hash[i + 1] && hash[i] == hash[i + 2]
  end
  nil
end

main