def spinlock(step_size)
  current_pos = 0
  value_after_zero = nil
  buffer_size = 1

  (1..2017).each do |i|
    current_pos = (current_pos + step_size) % buffer_size
    if current_pos == 0
      value_after_zero = i
    end
    current_pos += 1
    buffer_size += 1
  end

  value_after_zero
end

step_size = 380  # Replace with your actual puzzle input
result = spinlock(step_size)
puts result
