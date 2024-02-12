
file = File.open("input.txt")
two_count = 0
three_count = 0

file.each_line do |line|
  twos, threes = count_twos_and_threes(line)
  two_count += 1 if twos
  three_count += 1 if threes
end

checksum = two_count * three_count
puts checksum

def count_twos_and_threes(id : String) : Tuple(Bool, Bool)
  char_count = Hash(Char, Int32).new

  id.each_char do |char|
    char_count[char] ||= 0
    char_count[char] += 1
  end

  has_twos = false
  has_threes = false

  char_count.each_value do |count|
    has_twos = true if count == 2
    has_threes = true if count == 3
  end

  return has_twos, has_threes
end
