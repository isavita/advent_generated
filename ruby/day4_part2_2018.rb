
records = File.readlines('input.txt').sort

sleep_schedule = Hash.new { |h, k| h[k] = Array.new(60, 0) }

current_guard = nil
sleep_start = nil

records.each do |record|
  minute = record[15,2].to_i
  case record
  when /Guard #(\d+) begins shift/
    current_guard = $1.to_i
  when /falls asleep/
    sleep_start = minute
  when /wakes up/
    (sleep_start...minute).each { |m| sleep_schedule[current_guard][m] += 1 }
  end
end

guard_most_asleep = sleep_schedule.max_by { |_, minutes| minutes.sum }.first
minute_most_asleep = sleep_schedule[guard_most_asleep].each_with_index.max[1]

puts guard_most_asleep * minute_most_asleep

guard, minute = sleep_schedule.max_by { |_, minutes| minutes.max }.first
minute_most_frequently_asleep = sleep_schedule[guard].each_with_index.max[1]

puts guard * minute_most_frequently_asleep
