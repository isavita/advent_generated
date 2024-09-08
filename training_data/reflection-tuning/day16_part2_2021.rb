class Packet
  attr_reader :version, :type_id, :value, :sub_packets

  def initialize(binary)
    @version = binary[0, 3].to_i(2)
    @type_id = binary[3, 3].to_i(2)
    @sub_packets = []
    @remaining = parse(binary[6..-1])
  end

  def parse(binary)
    if @type_id == 4
      parse_literal(binary)
    else
      parse_operator(binary)
    end
  end

  def parse_literal(binary)
    value_bits = ""
    i = 0
    loop do
      group = binary[i, 5]
      value_bits += group[1, 4]
      i += 5
      break if group[0] == '0'
    end
    @value = value_bits.to_i(2)
    binary[i..-1]
  end

  def parse_operator(binary)
    length_type_id = binary[0]
    if length_type_id == '0'
      length = binary[1, 15].to_i(2)
      sub_packets_binary = binary[16, length]
      parse_sub_packets(sub_packets_binary)
      binary[(16 + length)..-1]
    else
      num_sub_packets = binary[1, 11].to_i(2)
      remaining = binary[12..-1]
      num_sub_packets.times do
        packet = Packet.new(remaining)
        @sub_packets << packet
        remaining = packet.remaining
      end
      remaining
    end
  end

  def parse_sub_packets(binary)
    while binary.length > 0
      packet = Packet.new(binary)
      @sub_packets << packet
      binary = packet.remaining
    end
  end

  def version_sum
    @version + @sub_packets.sum(&:version_sum)
  end

  def evaluate
    case @type_id
    when 0 then @sub_packets.sum(&:evaluate)
    when 1 then @sub_packets.map(&:evaluate).reduce(:*)
    when 2 then @sub_packets.map(&:evaluate).min
    when 3 then @sub_packets.map(&:evaluate).max
    when 4 then @value
    when 5 then @sub_packets[0].evaluate > @sub_packets[1].evaluate ? 1 : 0
    when 6 then @sub_packets[0].evaluate < @sub_packets[1].evaluate ? 1 : 0
    when 7 then @sub_packets[0].evaluate == @sub_packets[1].evaluate ? 1 : 0
    end
  end

  def remaining
    @remaining
  end
end

hex = File.read('input.txt').strip
binary = hex.chars.map { |c| c.to_i(16).to_s(2).rjust(4, '0') }.join

packet = Packet.new(binary)
puts "Part 1: #{packet.version_sum}"
puts "Part 2: #{packet.evaluate}"
