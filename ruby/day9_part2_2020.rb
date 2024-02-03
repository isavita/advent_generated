
numbers = File.readlines('input.txt').map(&:to_i)
invalid_number = 14360655

def find_contiguous_set(numbers, target)
  (0...numbers.length).each do |start|
    sum = 0
    (start...numbers.length).each do |i|
      sum += numbers[i]
      if sum == target
        return numbers[start..i].min + numbers[start..i].max
      elsif sum > target
        break
      end
    end
  end
end

puts find_contiguous_set(numbers, invalid_number)
