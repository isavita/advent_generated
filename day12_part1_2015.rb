
require 'json'

def sum_numbers(obj)
  if obj.is_a?(Integer)
    return obj
  elsif obj.is_a?(Array)
    return obj.map { |item| sum_numbers(item) }.sum
  elsif obj.is_a?(Hash)
    return obj.values.map { |value| sum_numbers(value) }.sum
  else
    return 0
  end
end

input = File.read('input.txt')
data = JSON.parse(input)

puts sum_numbers(data)
