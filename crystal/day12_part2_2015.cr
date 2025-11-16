
require "json"

def sum_numbers(data : JSON::Any) : Int32
  sum = 0
  case data.raw
  when Array
    data.as_a.each { |v| sum += sum_numbers(v) }
  when Hash
    return 0 if data.as_h.values.any? { |v| v.as_s? == "red" }
    data.as_h.each_value { |v| sum += sum_numbers(v) }
  when Int
    sum += data.as_i
  end
  sum
end

data = JSON.parse(File.read("input.txt"))
puts sum_numbers(data)
