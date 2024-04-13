def find_first_and_last_digit(line)
  digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
  first_digit = nil
  last_digit = nil

  line.each_char.with_index do |char, i|
    if char >= '0' && char <= '9'
      digit = char.to_i
      first_digit ||= digit
      last_digit = digit
    else
      digits.each_with_index do |digit_word, j|
        if line[i..-1].start_with?(digit_word)
          first_digit ||= j
          last_digit = j
          break
        end
      end
    end
  end

  [first_digit, last_digit]
end

sum = 0
File.open('input.txt', 'r') do |file|
  file.each_line do |line|
    first_digit, last_digit = find_first_and_last_digit(line.strip)
    sum += 10 * first_digit + last_digit if first_digit && last_digit
  end
end

puts sum