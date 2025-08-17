
require "file"

class Evaluator
  MAX = Int64::MAX
  MIN = Int64::MIN

  def self.evaluate(target : Int64, numbers : Array(Int64)) : Bool
    n = numbers.size
    return numbers[0] == target if n == 1
    ops = Array(Int32).new(n - 1, 0)
    loop do
      cur = numbers[0]
      ok = true
      (n - 1).times do |i|
        nxt = numbers[i + 1]
        case ops[i]
        when 0
          if nxt > 0 && cur > MAX - nxt
            ok = false; break
          elsif nxt < 0 && cur < MIN - nxt
            ok = false; break
          end
          cur += nxt
        when 1
          if nxt != 0
            if cur.abs > MAX / nxt.abs
              ok = false; break
            end
          end
          cur *= nxt
        when 2
          s = "#{cur}#{nxt}"
          begin
            cur = s.to_i64
          rescue
            ok = false; break
          end
        end
      end
      return true if ok && cur == target
      # increment ops as base-3
      k = ops.size - 1
      while k >= 0
        ops[k] += 1
        if ops[k] < 3
          break
        else
          ops[k] = 0
          k -= 1
        end
      end
      break if k < 0
    end
    false
  end
end

sum = 0_i64
File.open("input.txt") do |file|
  file.each_line do |line|
    line = line.strip
    next if line.empty?
    target_str, nums_str = line.split(':', 2)
    target = target_str.to_i64
    numbers = nums_str.split.map(&.to_i64)
    sum += target if Evaluator.evaluate(target, numbers)
  end
end
puts sum
