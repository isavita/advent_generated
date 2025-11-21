
require "json"

def compare(a : JSON::Any, b : JSON::Any) : Int32
  if a.raw.is_a?(Int) && b.raw.is_a?(Int)
    a.as_i <=> b.as_i
  elsif a.raw.is_a?(Int)
    compare(JSON.parse([a.as_i].to_json), b)
  elsif b.raw.is_a?(Int)
    compare(a, JSON.parse([b.as_i].to_json))
  else
    aa = a.as_a
    bb = b.as_a
    (0...{aa.size, bb.size}.min).each do |i|
      c = compare(aa[i], bb[i])
      return c if c != 0
    end
    aa.size <=> bb.size
  end
end

packets = [] of JSON::Any
File.read("input.txt").split("\n\n").each do |pair|
  sp = pair.split("\n")
  packets << JSON.parse(sp[0])
  packets << JSON.parse(sp[1])
end

div1 = JSON.parse("[[2]]")
div2 = JSON.parse("[[6]]")
packets << div1
packets << div2
packets.sort! { |a, b| compare(a, b) }
puts (packets.bsearch_index { |p| compare(p, div1) >= 0 }.not_nil! + 1) *
     (packets.bsearch_index { |p| compare(p, div2) >= 0 }.not_nil! + 1)
