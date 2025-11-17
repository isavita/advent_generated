
require "json"

def sign(n : Int)
  return 0 if n == 0
  n < 0 ? -1 : 1
end

def compare(a : JSON::Any, b : JSON::Any) : Int8
  anum = a.raw.is_a?(Int64) || a.raw.is_a?(Float64)
  bnum = b.raw.is_a?(Int64) || b.raw.is_a?(Float64)

  if anum && bnum
    sign(a.as_i - b.as_i).to_i8
  elsif anum
    compare(JSON::Any.new([a]), b)
  elsif bnum
    compare(a, JSON::Any.new([b]))
  else
    aa = a.as_a
    bb = b.as_a
    min_len = {aa.size, bb.size}.min
    i = 0
    while i < min_len
      c = compare(aa[i], bb[i])
      return c unless c == 0
      i += 1
    end
    sign(aa.size - bb.size).to_i8
  end
end

def main
  s = File.read("input.txt")
  sum = 0
  s.strip.split("\n\n").each_with_index do |pair, idx|
    sp = pair.split("\n")
    first = JSON.parse(sp[0])
    second = JSON.parse(sp[1])
    sum += idx + 1 if compare(first, second) == -1
  end
  puts sum
end

main
