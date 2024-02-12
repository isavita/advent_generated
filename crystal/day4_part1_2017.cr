
data = File.read("input.txt").chomp
passphrases = data.split("\n")
valid_count = 0

passphrases.each do |passphrase|
  words = passphrase.split(" ")
  word_set = {} of String => Bool

  valid = true
  words.each do |word|
    if word_set[word]?
      valid = false
      break
    end
    word_set[word] = true
  end

  valid_count += 1 if valid
end

puts valid_count
