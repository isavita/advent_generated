
def find_first_and_last_digit(line : String) : Tuple(Int32, Int32)
  digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

  first_digit = 0
  last_digit = 0
  line.each_char_with_index do |char, i|
    digit_str = char.to_s
    if digit_str >= "0" && digit_str <= "9"
      if first_digit == 0
        first_digit = digit_str.to_i
      end
      last_digit = digit_str.to_i
    else
      digits.each_with_index do |digit, j|
        if line[i..-1].starts_with?(digit)
          if first_digit == 0
            first_digit = j
          end
          last_digit = j
          break
        end
      end
    end
  end

  return first_digit, last_digit
end

sum = 0
File.open("input.txt") do |file|
  file.each_line do |line|
    first_digit, last_digit = find_first_and_last_digit(line)
    sum += 10 * first_digit + last_digit
  end
end

puts sum
