
def max_subseq(str, k)
  to_remove = str.length - k
  stack = []
  str.each_char do |c|
    while to_remove > 0 && !stack.empty? && stack[-1] < c
      stack.pop
      to_remove -= 1
    end
    stack << c
  end
  stack[0, k].join
end

if __FILE__ == $0
  total = 0
  File.foreach('input.txt') do |line|
    s = line.strip
    next if s.empty? || s.length < 12
    total += max_subseq(s, 12).to_i
  end
  puts total
end
