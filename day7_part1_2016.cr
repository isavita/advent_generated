
def supports_tls(ip : String) : Bool
  inside_brackets = ip.scan(/\[[a-z]+\]/)

  inside_brackets.each do |bracket_content|
    return false if contains_abba(bracket_content[0])
  end

  ip = ip.gsub(inside_brackets.map { |match| match[0] }.join, "-")
  contains_abba(ip)
end

def contains_abba(s : String) : Bool
  (0..s.size-4).any? do |i|
    s[i] != s[i+1] && s[i] == s[i+3] && s[i+1] == s[i+2]
  end
end

file = File.open("input.txt")
tls_count = 0

file.each_line do |line|
  tls_count += 1 if supports_tls(line)
end

puts tls_count
