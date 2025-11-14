
class Num
  property pos : Int32
  property val : Int64

  def initialize(@pos : Int32, @val : Int64)
  end
end

def mix(nums : Array(Num))
  n = nums.size - 1
  nums.each do |num|
    oldpos = num.pos
    newpos = ((oldpos.to_i64 + num.val) % n + n) % n
    if oldpos < newpos
      nums.each do |num2|
        num2.pos -= 1 if num2.pos > oldpos && num2.pos <= newpos
      end
    elsif newpos < oldpos
      nums.each do |num2|
        num2.pos += 1 if num2.pos >= newpos && num2.pos < oldpos
      end
    end
    num.pos = newpos.to_i
  end
end

def coords(nums : Array(Num)) : Int64
  l = nums.size
  zero_pos = nums.find { |n| n.val == 0 }.not_nil!.pos
  [1000, 2000, 3000].sum do |offset|
    target = (zero_pos + offset) % l
    nums.find { |n| n.pos == target }.not_nil!.val
  end
end

nums = File.read_lines("input.txt").map_with_index { |line, i| Num.new(i, line.to_i64) }
nums2 = nums.map { |num| Num.new(num.pos, 811589153_i64 * num.val) }

10.times { mix(nums2) }

puts coords(nums2)
