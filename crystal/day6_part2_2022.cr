file = File.open("input.txt")
str = file.gets_to_end.chomp

def first_n_unique(str, n)
  n.upto(str.size - 1) do |i|
    chars = str[i - n, n].bytes
    return i if chars.uniq.size == n
  end
  -1
end

puts first_n_unique(str, 14)