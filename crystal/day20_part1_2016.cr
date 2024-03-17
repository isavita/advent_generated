require "file"

struct IpRange
  getter start : UInt32
  getter end : UInt32

  def initialize(@start : UInt32, @end : UInt32)
  end
end

def read_ip_ranges(filename : String) : Array(IpRange)
  ranges = [] of IpRange
  File.each_line(filename) do |line|
    parts = line.split("-")
    start = parts[0].to_u32
    ending = parts[1].to_u32
    ranges << IpRange.new(start, ending)
  end
  ranges
end

def find_unblocked_ip(ranges : Array(IpRange)) : UInt32
  current_ip : UInt32 = 0
  ranges.each do |r|
    return current_ip if r.start > current_ip
    current_ip = r.end + 1 if r.end >= current_ip
  end
  current_ip
end

ip_ranges = read_ip_ranges("input.txt")
ip_ranges.sort! { |a, b| a.start <=> b.start }
unblocked_ip = find_unblocked_ip(ip_ranges)
puts unblocked_ip