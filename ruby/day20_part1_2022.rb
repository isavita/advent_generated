
class Num
  attr_accessor :pos, :val

  def initialize(pos, val)
    @pos = pos
    @val = val
  end
end

def mix(nums)
  n = nums.length - 1
  nums.each do |num|
    oldpos = num.pos
    newpos = ((oldpos + num.val) % n + n) % n
    if oldpos < newpos
      nums.each do |num2|
        if num2.pos > oldpos && num2.pos <= newpos
          num2.pos -= 1
        end
      end
    end
    if newpos < oldpos
      nums.each do |num2|
        if num2.pos >= newpos && num2.pos < oldpos
          num2.pos += 1
        end
      end
    end
    num.pos = newpos
  end
end

def coords(nums)
  l = nums.length
  zero_pos = 0
  nums.each do |num|
    if num.val == 0
      zero_pos = num.pos
      break
    end
  end
  sum = 0
  nums.each do |num|
    if num.pos == (zero_pos + 1000) % l || num.pos == (zero_pos + 2000) % l || num.pos == (zero_pos + 3000) % l
      sum += num.val
    end
  end
  sum
end

nums = []
File.foreach('input.txt') do |line|
  nums << Num.new(nums.length, line.chomp.to_i)
end

nums2 = nums.map { |num| Num.new(num.pos, 811589153 * num.val) }

mix(nums)
puts coords(nums)
