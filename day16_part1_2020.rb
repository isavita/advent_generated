
input = File.read('input.txt').split("\n\n")

rules = input[0].split("\n").map { |rule| rule.split(": ")[1].split(" or ").map { |range| Range.new(*range.split("-").map(&:to_i)) } }.flatten
nearby_tickets = input[2].split("\n")[1..-1].map { |ticket| ticket.split(",").map(&:to_i) }.flatten

puts nearby_tickets.select { |ticket| ticket if rules.none? { |range| range.include?(ticket) } }.sum
