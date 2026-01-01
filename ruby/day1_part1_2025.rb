#!/usr/bin/env ruby
def main
  pos = 50
  zero = 0
  File.foreach('input.txt') do |l|
    l = l.strip
    next if l.empty?
    dir = l[0]
    amt = l[1..-1].to_i
    pos = (pos + amt) % 100 if dir == 'R'
    pos = (pos - amt) % 100 if dir == 'L'
    raise "Invalid direction #{dir}" unless dir == 'R' || dir == 'L'
    zero += 1 if pos.zero?
  end
  puts "The password is: #{zero}"
end
main