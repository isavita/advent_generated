def main
  lines = File.readlines('input.txt', chomp: true)
  lines.map! { |l| l.end_with?("\r") ? l[0..-2] : l }

  if lines.empty?
    puts 'Grand total: 0'
    return
  end

  line_count = lines.size
  max_w = lines.map(&:length).max

  is_sep = Array.new(max_w, false)
  (0...max_w).each do |x|
    all_space = true
    line_count.times do |r|
      if x < lines[r].length && !lines[r][x].match?(/\s/)
        all_space = false
        break
      end
    end
    is_sep[x] = all_space
  end

  grand_total = 0
  in_block = false
  start = 0

  (0..max_w).each do |x|
    sep = (x == max_w) ? true : is_sep[x]
    if !sep
      unless in_block
        in_block = true
        start = x
      end
    else
      if in_block
        nums = []
        op = '+'
        (start..(x - 1)).each do |c|
          sb = +''
          line_count.times do |r|
            if c < lines[r].length
              ch = lines[r][c]
              if ch =~ /\d/
                sb << ch
              elsif ch == '+' || ch == '*'
                op = ch
              end
            end
          end
          nums << sb unless sb.empty?
        end

        unless nums.empty?
          block_val = if op == '*'
            nums.map { |s| s.to_i }.reduce(1, :*)
          else
            nums.map { |s| s.to_i }.reduce(0, :+)
          end
          grand_total += block_val
        end

        in_block = false
      end
    end
  end

  puts "Grand total: #{grand_total}"
end

main if __FILE__ == $0