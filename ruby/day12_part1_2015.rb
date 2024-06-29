
require 'json'

def sum_numbers(data)
  case data
  when Array
    data.sum { |v| sum_numbers(v) }
  when Hash
    data.values.sum { |v| sum_numbers(v) }
  when Numeric
    data.to_i
  else
    0
  end
end

json_data = JSON.parse(File.read('input.txt'))
puts sum_numbers(json_data)
