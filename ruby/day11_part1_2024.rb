
def even_digits(s)
  s.length.even?
end

def trim_leading_zeros(s)
  s.sub(/^0+/, '')
end

stones = File.read("input.txt").strip.split

25.times do
  next_stones = []
  stones.each do |s|
    if s == "0"
      next_stones << "1"
    elsif even_digits(s)
      mid = s.length / 2
      left = trim_leading_zeros(s[0, mid])
      right = trim_leading_zeros(s[mid..-1])
      left = "0" if left.empty?
      right = "0" if right.empty?
      next_stones << left << right
    else
      next_stones << (s.to_i * 2024).to_s
    end
  end
  stones = next_stones
end

puts stones.length
