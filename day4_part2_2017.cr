
require "file"

data = File.read("input.txt").chomp
passphrases = data.split("\n")
valid_count = 0

def sort_string(w : String) : String
  w.split("").sort.join("")
end

passphrases.each do |passphrase|
  words = passphrase.split(" ")
  word_set = {} of String => Bool

  valid = true
  words.each do |word|
    sorted_word = sort_string(word)
    if word_set[sorted_word]?
      valid = false
      break
    end
    word_set[sorted_word] = true
  end

  valid_count += 1 if valid
end

puts valid_count
