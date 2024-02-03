
ranges = File.readlines('input.txt').map { |line| line.split('-').map(&:to_i) }.sort_by(&:first)

merged_ranges = []
ranges.each do |range|
  if merged_ranges.empty? || merged_ranges.last[1] < range[0] - 1
    merged_ranges << range
  else
    merged_ranges.last[1] = [merged_ranges.last[1], range[1]].max
  end
end

lowest_ip = merged_ranges.first[1] + 1
allowed_ips = 4294967296 - merged_ranges.sum { |range| range[1] - range[0] + 1 }

puts lowest_ip
puts allowed_ips
