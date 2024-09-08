# Read and parse input
input = File.read('input.txt').split("\n\n")
rules = input[0].split("\n")
nearby_tickets = input[2].split("\n")[1..]

# Create a set of all valid numbers
valid_numbers = Set.new
rules.each do |rule|
  rule.scan(/(\d+)-(\d+)/).each do |start, end_|
    valid_numbers.merge(start.to_i..end_.to_i)
  end
end

# Check nearby tickets and sum invalid values
error_rate = nearby_tickets.sum do |ticket|
  ticket.split(',').sum do |number|
    num = number.to_i
    valid_numbers.include?(num) ? 0 : num
  end
end

puts error_rate
