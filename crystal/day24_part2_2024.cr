
struct Gate
  getter a : String
  getter op : String
  getter b : String

  def initialize(@a : String, @op : String, @b : String)
  end
end

def parse(input : String) : Array({Gate, String})?
  parts = input.split("\n\n")
  return nil unless parts.size == 2

  gates = [] of {Gate, String}
  parts[1].lines.each do |line|
    next if line.empty?
    parts2 = line.split(" -> ")
    next unless parts2.size == 2
    gate_parts = parts2[0].split(" ")
    next unless gate_parts.size == 3
    gates << {Gate.new(gate_parts[0], gate_parts[1], gate_parts[2]), parts2[1]}
  end
  gates
end

def create_lookups(gates : Array({Gate, String})) : Tuple(Hash(String, Gate), Hash(String, String))
  lookup = Hash(String, Gate).new
  reverse_lookup = Hash(String, String).new

  gates.each do |g|
    lookup[g[1]] = g[0]
    inputs = [g[0].a, g[0].b].sort
    key = "#{inputs[0]}_#{g[0].op}_#{inputs[1]}"
    reverse_lookup[key] = g[1]
  end
  {lookup, reverse_lookup}
end

def swap(pairs : Array({String, String}), gates : Array({Gate, String}), a : String, b : String)
  pairs << {a, b}
  gates.size.times do |i|
    if gates[i][1] == a
      gates[i] = {gates[i][0], b}
    elsif gates[i][1] == b
      gates[i] = {gates[i][0], a}
    end
  end
end

def get_reverse_lookup_key(a : String, op : String, b : String) : String
  inputs = [a, b].sort
  "#{inputs[0]}_#{op}_#{inputs[1]}"
end

def solution(gates : Array({Gate, String})) : String
  pairs = [] of {String, String}
  num_z = gates.count { |g| g[1].starts_with?("z") }

  while pairs.size < 4
    adder = ""
    carry = ""
    lookup, reverse_lookup = create_lookups(gates)

    num_z.times do |i|
      xi = "x#{i.to_s.rjust(2, '0')}"
      yi = "y#{i.to_s.rjust(2, '0')}"
      zi = "z#{i.to_s.rjust(2, '0')}"

      if i == 0
        adder = reverse_lookup[get_reverse_lookup_key(xi, "XOR", yi)]? || ""
        carry = reverse_lookup[get_reverse_lookup_key(xi, "AND", yi)]? || ""
      else
        bit = reverse_lookup[get_reverse_lookup_key(xi, "XOR", yi)]? || ""
        if !bit.empty?
          adder = reverse_lookup[get_reverse_lookup_key(bit, "XOR", carry)]? || ""
          if !adder.empty?
            c1 = reverse_lookup[get_reverse_lookup_key(xi, "AND", yi)]? || ""
            c2 = reverse_lookup[get_reverse_lookup_key(bit, "AND", carry)]? || ""
            carry = reverse_lookup[get_reverse_lookup_key(c1, "OR", c2)]? || ""
          end
        end
      end

      if adder.empty?
        gate = lookup[zi]?
        next unless gate
        bit_key = get_reverse_lookup_key(xi, "XOR", yi)
        bit = reverse_lookup[bit_key]? || ""
        if reverse_lookup[get_reverse_lookup_key(gate.a, "XOR", carry)]?
          swap(pairs, gates, bit, gate.a)
          break
        elsif reverse_lookup[get_reverse_lookup_key(gate.b, "XOR", carry)]?
          swap(pairs, gates, bit, gate.b)
          break
        end
      elsif adder != zi
        swap(pairs, gates, adder, zi)
        break
      end
    end
  end

  result = pairs.flat_map { |p| [p[0], p[1]] }.sort
  result.join(",")
end

input = File.read("input.txt")
gates = parse(input)
if gates.nil?
  puts "Error parsing input"
  exit
end
puts solution(gates)
