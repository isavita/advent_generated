lines = File.readlines('input.txt', chomp: true)
maxw = lines.map(&:length).max || 0
grand = 0
in_block = false
start_col = 0

def separator?(lines, col)
  lines.each do |line|
    return false if col < line.length && !line[col].match?(/\s/)
  end
  true
end

def block_value(lines, sc, ec)
  nums = []
  op = 0
  lines.each do |line|
    next if sc >= line.length
    seg = line[sc..ec] || ''
    seg = seg.strip
    next if seg.empty?
    if seg == '+'
      op = 1
    elsif seg == '*'
      op = 2
    else
      nums << seg.to_i
    end
  end
  return 0 if nums.empty?
  case op
  when 1
    nums.sum
  when 2
    nums.inject(1) { |a, b| a * b }
  else
    nums.size == 1 ? nums[0] : 0
  end
end

(0...maxw).each do |col|
  if !separator?(lines, col)
    unless in_block
      in_block = true
      start_col = col
    end
  else
    if in_block
      grand += block_value(lines, start_col, col - 1)
      in_block = false
    end
  end
end

grand += block_value(lines, start_col, maxw - 1) if in_block

puts "Grand total: #{grand}"