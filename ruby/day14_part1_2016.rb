
require 'digest'

def find_keys(salt)
  index = 0
  keys = []
  
  while keys.length < 64
    hash = Digest::MD5.hexdigest("#{salt}#{index}")
    
    if hash =~ /(.)\1\1/
      char = $1
      if (1..1000).any? { |i| Digest::MD5.hexdigest("#{salt}#{index + i}") =~ /#{char}{5}/ }
        keys << index
      end
    end
    
    index += 1
  end
  
  keys.last
end

input = File.read('input.txt').strip
puts find_keys(input)
