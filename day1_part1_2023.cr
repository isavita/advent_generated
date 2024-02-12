# Define a method to extract the calibration value from a single line
def extract_calibration_value(line : String) : Int32?
  # Use regular expressions to find the first and last digits in the line
  first_digit_match = line.match(/\d/)
  last_digit_match = line.match(/\d(?=[^\d]*\z)/)

  # If both the first and last digits are found, combine them into a two-digit number
  if first_digit_match && last_digit_match
    (first_digit_match[0] + last_digit_match[0]).to_i
  else
    nil # Return nil if no valid calibration value is found
  end
end

# Initialize a variable to keep track of the total sum of calibration values
total_sum = 0

# Read from the file 'input.txt'
File.open("input.txt").each_line do |line|
  calibration_value = extract_calibration_value(line.strip)
  total_sum += calibration_value if calibration_value
end

# Print the total sum to stdout
puts total_sum

