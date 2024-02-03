
def supports_tls?(ip)
  outside_brackets, inside_brackets = ip.split(/\[|\]/).partition.with_index { |_, i| i.even? }
  has_abba = ->(s) { s.each_char.each_cons(4).any? { |a, b, c, d| a != b && a == d && b == c } }
  has_abba[outside_brackets.join(' ')] && !has_abba[inside_brackets.join(' ')]
end

puts File.readlines('input.txt').count { |ip| supports_tls?(ip.chomp) }
