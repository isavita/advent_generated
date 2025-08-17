
def sum_numbers(str : String) : Int32
  sum = 0_i32
  num = 0_i32
  negative = false

  str.each_byte do |c|
    case c
    when '-'.ord
      negative = true
    when '0'.ord..'9'.ord
      num = num * 10 + (c - '0'.ord)
    else
      num = -num if negative
      sum += num
      num = 0
      negative = false
    end
  end

  num = -num if negative
  sum + num
end

input = File.read("input.txt")
puts sum_numbers(input)
