
def calculate_max_joltage(bank)
  9.downto(0) do |d1|
    idx = bank.index(d1.to_s)
    next if idx.nil? || idx >= bank.length - 1

    max_d2 = -1
    bank[(idx + 1)..-1].each_char do |ch|
      next unless ch >= '0' && ch <= '9'
      v = ch.ord - 48
      max_d2 = v if v > max_d2
      break if max_d2 == 9
    end

    return d1 * 10 + max_d2 if max_d2 != -1
  end
  0
end

total = 0
if File.exist?('input.txt')
  File.foreach('input.txt') do |line|
    s = line.strip
    total += calculate_max_joltage(s) unless s.empty?
  end
end

puts "Total output joltage: #{total}"
