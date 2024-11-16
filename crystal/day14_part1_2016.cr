
require "digest/md5"

class OneTimePad
  def initialize(salt : String)
    @salt = salt
    @index = 0
    @keys = [] of Int32
  end

  def find_64th_key
    while @keys.size < 64
      hash = md5(@index)
      if triplet = find_triplet(hash)
        (1..1000).each do |offset|
          next_hash = md5(@index + offset)
          if find_quintuplet(next_hash, triplet)
            @keys << @index
            break
          end
        end
      end
      @index += 1
    end
    @keys[63]
  end

  private def md5(index : Int32) : String
    Digest::MD5.hexdigest "#{@salt}#{index}"
  end

  private def find_triplet(hash : String) : Char?
    hash.each_char.with_index do |char, i|
      return char if i + 2 < hash.size && char == hash[i + 1] && char == hash[i + 2]
    end
    nil
  end

  private def find_quintuplet(hash : String, triplet : Char) : Bool
    hash.each_char.with_index do |char, i|
      return true if i + 4 < hash.size && char == triplet && char == hash[i + 1] && char == hash[i + 2] && char == hash[i + 3] && char == hash[i + 4]
    end
    false
  end
end

# Read the salt from the input file
salt = File.read("input.txt").strip

# Create an instance of OneTimePad and find the 64th key
pad = OneTimePad.new(salt)
puts pad.find_64th_key
