require "file_utils"

def find_abas(s)
  abas = [] of String
  (0...s.size - 2).each do |i|
    abas << s[i, 3] if s[i] != s[i + 1] && s[i] == s[i + 2]
  end
  abas
end

def supports_ssl(ip)
  inside_brackets = /\[[a-z]+\]/
  bracket_contents = ip.scan(inside_brackets).map { |match| match[0] } # Get matched strings

  ip = ip.gsub(inside_brackets, "-")
  find_abas(ip).each do |aba|
    bab = "#{aba[1]}#{aba[0]}#{aba[1]}"
    return true if bracket_contents.any? { |bc| bc.includes?(bab) }
  end

  false
end

ssl_count = 0
File.open("input.txt") do |file|
  file.each_line do |line|
    ssl_count += 1 if supports_ssl(line.chomp)
  end
end

puts ssl_count