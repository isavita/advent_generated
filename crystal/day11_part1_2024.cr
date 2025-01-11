
def even_digits(s : String) : Bool
  s.size.even?
end

def trim_leading_zeros(s : String) : String
  while s.size > 1 && s[0] == '0'
    s = s[1..]
  end
  s
end

data = File.read("input.txt").strip
stones = data.split

25.times do
  next_stones = [] of String
  stones.each do |s|
    if s == "0"
      next_stones << "1"
    elsif even_digits(s)
      mid = s.size // 2
      left = trim_leading_zeros(s[0...mid])
      right = trim_leading_zeros(s[mid..])
      left = "0" if left.empty?
      right = "0" if right.empty?
      next_stones << left << right
    else
      n = s.to_i64
      next_stones << (n * 2024).to_s
    end
  end
  stones = next_stones
end

puts stones.size
