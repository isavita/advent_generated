
class Day14
  def self.generate_addresses(mask : String, address : Int64) : Array(Int64)
    floating = [] of Int32
    addresses = [] of Int64

    # Apply mask '1's and collect floating positions
    mask.chars.each_with_index do |bit, i|
      case bit
      when '1'
        address |= (1_i64 << (35 - i))
      when 'X'
        floating << (35 - i)
      end
    end

    # Generate all combinations for the floating bits
    count = 1 << floating.size
    (0...count).map do |i|
      mod_address = address
      floating.each_with_index do |pos, j|
        if i & (1 << j) == 0
          mod_address &= ~(1_i64 << pos)  # Clear bit
        else
          mod_address |= (1_i64 << pos)   # Set bit
        end
      end
      mod_address
    end
  end

  def self.solve
    mem = {} of Int64 => Int64
    mask = ""

    File.each_line("input.txt") do |line|
      if line.starts_with?("mask = ")
        mask = line[7..-1]
      elsif line =~ /mem\[(\d+)] = (\d+)/
        address = $1.to_i64
        value = $2.to_i64
        generate_addresses(mask, address).each do |addr|
          mem[addr] = value
        end
      end
    end

    mem.values.sum
  end
end

puts Day14.solve
