
def main
  guards = {} of Int32 => Array(Int32)
  current_guard = -1
  falls_asleep = -1

  lines = File.read_lines("input.txt")
  lines.sort!

  lines.each do |line|
    if line.includes?('#')
      current_guard = line.split[3][1..].to_i
      guards[current_guard] ||= Array.new(60, 0)
    elsif line.includes?("falls asleep")
      falls_asleep = line.split[1][3..4].to_i
    elsif line.includes?("wakes up")
      wakes_up = line.split[1][3..4].to_i
      falls_asleep.upto(wakes_up - 1) do |i|
        guards[current_guard][i] += 1
      end
    end
  end

  max_sleep = guards.max_of { |_, v| v.sum }
  sleepiest_guard = guards.find { |_, v| v.sum == max_sleep }.not_nil!.first

  most_common_minute = guards[sleepiest_guard].index(guards[sleepiest_guard].max).not_nil!

  result_part1 = sleepiest_guard * most_common_minute
  puts result_part1

  most_frequent_minute = guards.max_of do |guard, minutes|
    max_minute_count = minutes.max
    {max_minute_count, guard, minutes.index(max_minute_count).not_nil!}
  end

  result_part2 = most_frequent_minute[1] * most_frequent_minute[2]
  puts result_part2
end

main
