
require "string_scanner"

def alphabetize(s)
  s.chars.sort!.join
end

def contains_all_chars(larger, smaller)
  return false if smaller.size > larger.size
  li = 0
  smaller.each_char do |c|
    li = larger.index(c, li) || return false
    li += 1
  end
  true
end

total_sum = 0_i64
File.read_lines("input.txt").each do |line|
  parts = line.split
  mapping = Array.new(10) { "" }
  five = [] of String
  six  = [] of String

  10.times do |i|
    p = alphabetize(parts[i])
    case p.size
    when 2 then mapping[1] = p
    when 4 then mapping[4] = p
    when 3 then mapping[7] = p
    when 7 then mapping[8] = p
    when 5 then five << p
    when 6 then six  << p
    end
  end

  six.each do |p|
    if !contains_all_chars(p, mapping[1])
      mapping[6] = p
    elsif contains_all_chars(p, mapping[4])
      mapping[9] = p
    else
      mapping[0] = p
    end
  end

  five.each do |p|
    if contains_all_chars(p, mapping[1])
      mapping[3] = p
    elsif contains_all_chars(mapping[6], p)
      mapping[5] = p
    else
      mapping[2] = p
    end
  end

  value = 0
  4.times do |i|
    pattern = alphabetize(parts[11 + i])
    10.times do |d|
      if pattern == mapping[d]
        value = value * 10 + d
        break
      end
    end
  end
  total_sum += value
end

puts total_sum
