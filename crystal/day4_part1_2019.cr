
File.open("input.txt") do |file|
  range_str = file.gets
  unless range_str
    puts "No input range provided in the file."
    exit
  end

  start, finish = range_str.to_s.split("-").map(&.to_i)

  count = 0
  (start..finish).each do |i|
    if has_double_and_increasing_digits(i.to_s)
      count += 1
    end
  end

  puts count
end

def has_double_and_increasing_digits(s : String) : Bool
  has_double = false
  (0...s.size-1).each do |i|
    if s[i] == s[i+1]
      has_double = true
    end
    if s[i] > s[i+1]
      return false
    end
  end
  has_double
end
