
require "big"

STEPS = 75

def trim(s)
  t = s.lstrip('0')
  t.empty? ? "0" : t
end

def split_stone(s)
  mid = s.size // 2
  {trim(s[0, mid]), trim(s[mid..-1])}
end

def multiply_by_2024(s)
  (BigInt.new(s) * 2024).to_s
end

def main
  line = File.read("input.txt").strip
  cur = Hash(String, UInt64).new(0_u64)
  line.split(/\s+/).each { |tok| cur[tok] = cur[tok] + 1_u64 }
  nxt = Hash(String, UInt64).new(0_u64)

  STEPS.times do
    nxt.clear
    cur.each do |stone, cnt|
      if stone == "0"
        nxt["1"] = nxt["1"] + cnt
      elsif stone.size.even?
        left, right = split_stone(stone)
        nxt[left] = nxt[left] + cnt
        nxt[right] = nxt[right] + cnt
      else
        ns = multiply_by_2024(stone)
        nxt[ns] = nxt[ns] + cnt
      end
    end
    cur, nxt = nxt, cur
  end

  total = cur.values.reduce(0_u64) { |a, b| a + b }
  puts total
end

main
