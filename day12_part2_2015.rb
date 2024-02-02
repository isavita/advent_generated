
require 'json'

def sum_numbers(json, ignore_red=false)
  if json.is_a?(Hash)
    return 0 if ignore_red && json.value?("red")
    return json.values.sum { |value| sum_numbers(value, ignore_red) }
  elsif json.is_a?(Array)
    return json.sum { |value| sum_numbers(value, ignore_red) }
  elsif json.is_a?(Integer)
    return json
  else
    return 0
  end
end

input = File.read('input.txt')
data = JSON.parse(input)

puts sum_numbers(data)
puts sum_numbers(data, true)
