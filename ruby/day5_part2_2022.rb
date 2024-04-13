def read_all(path)
  File.read(path).strip
end

def move(st, steps)
  stacks = st.map { |s| s.reverse }
  steps.each do |step|
    n, from, to = step.scan(/\d+/).map(&:to_i)
    from -= 1
    to -= 1
    stacks[to].concat(stacks[from].pop(n))
  end
  stacks.map(&:last).join
end

s = read_all("input.txt").split("\n\n")
input = s[0].split("\n")
stacks = Array.new((input[0].length + 1) / 4) { [] }
input.each do |line|
  line.bytes.each_with_index do |b, i|
    stacks[(i - 1) / 4] << b.chr if b.between?('A'.ord, 'Z'.ord)
  end
end

steps = s[1].split("\n")
puts move(stacks, steps)