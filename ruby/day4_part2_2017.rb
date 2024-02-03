passphrases = File.readlines('input.txt').map(&:chomp)

valid_passphrases = passphrases.select do |passphrase|
  words = passphrase.split(' ')
  words == words.uniq && words.map { |word| word.chars.sort.join }.uniq.size == words.size
end

puts valid_passphrases.size