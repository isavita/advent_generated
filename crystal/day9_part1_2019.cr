
module Intcode
  def self.run(memory : Hash(Int64, Int64)) : Int64
    output = 0_i64
    ip = 0_i64
    rb = 0_i64
    while true
      op = memory[ip] % 100
      modes = (memory[ip] // 100).to_s.chars.reverse
      getm = ->(i : Int32) { modes[i]? || '0' }
      read = ->(offset : Int32) do
        m = getm.call(offset - 1)
        v = memory[ip + offset]
        case m
        when '0' then memory[v]?
        when '1' then v
        when '2' then memory[rb + v]?
        else           0_i64
        end || 0_i64
      end
      write = ->(offset : Int32, val : Int64) do
        m = getm.call(offset - 1)
        v = memory[ip + offset]
        case m
        when '0' then memory[v] = val
        when '2' then memory[rb + v] = val
        end
      end
      case op
      when 1
        write.call(3, read.call(1) + read.call(2))
        ip += 4
      when 2
        write.call(3, read.call(1) * read.call(2))
        ip += 4
      when 3
        write.call(1, 1_i64)
        ip += 2
      when 4
        output = read.call(1)
        ip += 2
      when 5
        ip = read.call(1) != 0 ? read.call(2) : ip + 3
      when 6
        ip = read.call(1) == 0 ? read.call(2) : ip + 3
      when 7
        write.call(3, read.call(1) < read.call(2) ? 1_i64 : 0_i64)
        ip += 4
      when 8
        write.call(3, read.call(1) == read.call(2) ? 1_i64 : 0_i64)
        ip += 4
      when 9
        rb += read.call(1)
        ip += 2
      when 99
        return output
      else
        raise "unknown opcode #{op}"
      end
    end
  end
end

memory = Hash(Int64, Int64).new(0_i64)
File.read("input.txt").split(',').each_with_index do |v, i|
  memory[i.to_i64] = v.to_i64
end
puts Intcode.run(memory)
