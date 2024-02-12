
def is_valid_password(password : Int32) : Bool
  s = password.to_s
  has_double = false

  (0...s.size-1).each do |i|
    if s[i] > s[i+1]
      return false
    end

    if s[i] == s[i+1]
      if (i == 0 || s[i] != s[i-1]) && (i+2 >= s.size || s[i] != s[i+2])
        has_double = true
      end
    end
  end

  has_double
end

data = File.read("input.txt").chomp
ranges = data.split("-")
start = ranges[0].to_i
finish = ranges[1].to_i

count = 0
(start..finish).each do |i|
  count += 1 if is_valid_password(i)
end

puts count
