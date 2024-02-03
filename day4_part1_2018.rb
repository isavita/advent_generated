
data = File.readlines('input.txt').map(&:chomp)

guards = {}
current_guard = nil
sleep_start = nil

data.sort.each do |line|
  case line
  when /Guard #(\d+)/
    current_guard = $1.to_i
    guards[current_guard] ||= Hash.new(0)
  when /(\d+):(\d+)] falls asleep/
    sleep_start = $2.to_i
  when /(\d+):(\d+)] wakes up/
    (sleep_start...$2.to_i).each { |min| guards[current_guard][min] += 1 }
  end
end

guard_id = guards.max_by { |_, mins| mins.values.sum }[0]
minute = guards[guard_id].max_by { |_, v| v }[0]

puts guard_id * minute
