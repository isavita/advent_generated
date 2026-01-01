
def main
  ranges = []
  File.readlines('input.txt', chomp: true).each do |line|
    s = line.strip
    next if s.empty?
    a_str, b_str = s.split('-', 2)
    next unless b_str
    a = a_str.to_i
    b = b_str.to_i
    a, b = b, a if a > b
    ranges << [a, b]
  end

  if ranges.empty?
    puts 'Total fresh IDs: 0'
    return
  end

  ranges.sort_by! { |r| [r[0], r[1]] }

  total = 0
  cur_min, cur_max = ranges[0]

  ranges[1..-1].each do |r|
    if r[0] <= cur_max
      cur_max = r[1] if r[1] > cur_max
    else
      total += cur_max - cur_min + 1
      cur_min, cur_max = r
    end
  end
  total += cur_max - cur_min + 1

  puts "Total fresh IDs: #{total}"
end

main
