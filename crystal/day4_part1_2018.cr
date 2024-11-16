
# Read input from file
input = File.read_lines("input.txt").sort

# Data structures to track guard sleep patterns
guard_sleep_times = {} of Int32 => Array(Int32)
guard_total_sleep = {} of Int32 => Int32
current_guard = 0
sleep_start = 0

# Process input records
input.each do |record|
  case record
  when /Guard #(\d+) begins shift/
    current_guard = $1.to_i
    guard_sleep_times[current_guard] ||= Array.new(60, 0)
    guard_total_sleep[current_guard] ||= 0
  when /falls asleep/
    sleep_start = record[15..16].to_i
  when /wakes up/
    wake_time = record[15..16].to_i
    (sleep_start...wake_time).each do |minute|
      guard_sleep_times[current_guard][minute] += 1
    end
    guard_total_sleep[current_guard] += wake_time - sleep_start
  end
end

# Strategy 1: Find guard with most total sleep time
sleepiest_guard = guard_total_sleep.max_by { |_, total| total }[0]

# Find minute most frequently slept by sleepiest guard
sleepiest_minute = guard_sleep_times[sleepiest_guard].index(guard_sleep_times[sleepiest_guard].max).not_nil!

# Calculate result
result = sleepiest_guard * sleepiest_minute

puts result
