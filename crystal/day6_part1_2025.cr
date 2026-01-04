require "big"

def process_block(lines, start, fin)
  op = "" 
  nums = [] of BigInt
  lines.each do |line|
    next if start >= line.size
    seg = line[start..{fin, line.size - 1}.min].strip
    next if seg.empty?
    if seg == "+" || seg == "*"
      op = seg
    else
      begin
        nums << BigInt.new(seg)
      rescue
      end
    end
  end
  return BigInt.new(0) if nums.empty?
  return nums.reduce(BigInt.new(0)) { |a, b| a + b } if op == "+"
  return nums.reduce(BigInt.new(1)) { |a, b| a * b } if op == "*"
  return nums[0] if nums.size == 1
  BigInt.new(0)
end

def main
  lines = File.read_lines("input.txt")
  if lines.empty?
    puts "Grand total: 0"
    return
  end
  max_width = lines.map(&.size).max || 0
  is_sep = Array.new(max_width, true)
  (0...max_width).each do |x|
    all_space = true
    lines.each do |line|
      if x < line.size && !line[x].whitespace?
        all_space = false
        break
      end
    end
    is_sep[x] = all_space
  end
  total = BigInt.new(0)
  in_block = false
  start = 0
  (0...max_width).each do |x|
    if !is_sep[x]
      unless in_block
        in_block = true
        start = x
      end
    else
      if in_block
        total += process_block(lines, start, x - 1)
        in_block = false
      end
    end
  end
  total += process_block(lines, start, max_width - 1) if in_block
  puts "Grand total: #{total}"
end

main