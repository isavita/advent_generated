def add_snailfish(a, b)
  "[#{a},#{b}]"
end

def reduce(s)
  loop do
    s, changed = explode(s)
    next if changed
    s, changed = split(s)
    break unless changed
  end
  s
end

def explode(s)
  depth = 0
  s.chars.each_with_index do |c, i|
    depth += 1 if c == '['
    depth -= 1 if c == ']'
    if depth == 5
      pair = s[i..-1].match(/\[(\d+),(\d+)\]/)
      return s, false unless pair # Handle unexpected input gracefully
      left, right = pair[1].to_i, pair[2].to_i
      before = s[0...i].gsub(/(\d+)(?!.*\d)/) { |m| m.to_i + left }
      after = s[(i + pair[0].length)..-1].gsub(/(\d+)/) { |m| m.to_i + right; break }
      return "#{before}0#{after}", true
    end
  end
  [s, false]
end

def split(s)
  s.gsub!(/\d{2,}/) do |m|
    n = m.to_i
    "[#{n / 2},#{(n + 1) / 2}]"
  end
  [s, $~] # $~ is nil if no substitution was made
end

def magnitude(s)
  while s.include?('[')
    s.gsub!(/\[(\d+),(\d+)\]/) { 3 * $1.to_i + 2 * $2.to_i }
  end
  s.to_i
end

# Part 1
input = File.readlines('input.txt').map(&:strip)
result = input.reduce { |sum, num| reduce(add_snailfish(sum, num)) }
puts "Part 1: #{magnitude(result)}"

# Part 2
max_magnitude = input.permutation(2).map do |a, b|
  magnitude(reduce(add_snailfish(a, b)))
end.max

puts "Part 2: #{max_magnitude}"
