
require 'json'

def compare(a, b)
  if a.is_a?(Numeric) && b.is_a?(Numeric)
    (a - b).round
  elsif a.is_a?(Numeric)
    compare([a], b)
  elsif b.is_a?(Numeric)
    compare(a, [b])
  else
    aa, bb = a, b
    (0...[aa.length, bb.length].min).each do |i|
      c = compare(aa[i], bb[i])
      return c if c != 0
    end
    aa.length - bb.length
  end
end

def read_all(path)
  File.read(path)
end

def main
  s = read_all('input.txt')
  packets = []
  s.split("\n\n").each do |pair|
    sp = pair.split("\n")
    first = JSON.parse(sp[0])
    second = JSON.parse(sp[1])
    packets << first << second
  end

  divider1 = JSON.parse('[[2]]')
  divider2 = JSON.parse('[[6]]')
  packets << divider1 << divider2
  packets.sort! { |a, b| compare(a, b) }
  divider1_pos = packets.bsearch_index { |x| compare(x, divider1) >= 0 } + 1
  divider2_pos = packets.bsearch_index { |x| compare(x, divider2) >= 0 } + 1
  puts divider1_pos * divider2_pos
end

main
