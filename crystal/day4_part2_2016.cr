
require "file"

struct LetterCount
  include Comparable(LetterCount)

  property letter : Char
  property count : Int32

  def initialize(@letter, @count)
  end

  def <=>(other : LetterCount)
    if count == other.count
      letter <=> other.letter
    else
      other.count <=> count
    end
  end
end

def is_real_room(room : String) : Bool
  parts = room.split("[")
  checksum = parts[1].chomp("]")
  encrypted_name = parts[0].split("-")[0...-1]

  letter_counts = {} of Char => Int32
  encrypted_name.each do |part|
    part.each_char do |letter|
      letter_counts[letter] = (letter_counts[letter]? || 0) + 1
    end
  end

  counts = letter_counts.map { |letter, count| LetterCount.new(letter, count) }.sort!

  checksum.each_char.with_index do |char, i|
    return false if char != counts[i].letter
  end

  true
end

def get_sector_id(room : String) : Int32
  parts = room.split("-")
  parts[-1].split("[")[0].to_i
end

def decrypt_name(room : String) : String
  parts = room.split("-")
  sector_id = parts[-1].split("[")[0].to_i

  parts[0...-1].map do |part|
    part.chars.map do |letter|
      letter == '-' ? ' ' : ((letter.ord - 'a'.ord + sector_id) % 26 + 'a'.ord).chr
    end.join
  end.join(" ")
end

File.open("input.txt") do |file|
  file.each_line do |line|
    if is_real_room(line)
      decrypted_name = decrypt_name(line)
      if decrypted_name.includes?("northpole object")
        puts get_sector_id(line)
        break
      end
    end
  end
end
