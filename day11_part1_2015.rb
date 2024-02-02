
def valid_password?(password)
  return false if password.match(/[iol]/)
  return false unless password.scan(/(.)\1/).uniq.size >= 2
  return false unless password.chars.each_cons(3).any? { |a, b, c| a.ord + 1 == b.ord && b.ord + 1 == c.ord }
  true
end

def next_password(password)
  loop do
    password.succ!
    break if valid_password?(password)
  end
  password
end

input = File.read("input.txt").strip
puts next_password(input)
