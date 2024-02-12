
require "set"

def first_n_unique(s : String, n : Int) : Int32
  i = n
  while i <= s.size
    b = s[i - n, n].chars.to_a
    if b.size == b.to_set.size
      return i
    end
    i += 1
  end
  return -1
end

def read_all(path : String) : String
  return File.read(path).strip
end

def set_of(b : Array(Char)) : Array(Char)
  return b.to_set.to_a
end

s = read_all("input.txt")
puts first_n_unique(s, 4)
