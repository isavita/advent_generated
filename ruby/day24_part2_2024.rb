
def get_reverse_lookup_key(a, op, b)
  inputs = [a, b].sort
  "#{inputs[0]}_#{op}_#{inputs[1]}"
end

def create_lookups(gates)
  lookup = {}
  reverse_lookup = {}

  gates.each do |g|
    output = g[:output]
    gate = g[:gate]
    lookup[output] = gate

    inputs = [gate[:a], gate[:b]]
    inputs.sort!
    key = "#{inputs[0]}_#{gate[:op]}_#{inputs[1]}"
    reverse_lookup[key] = output
  end

  [lookup, reverse_lookup]
end

def swap(pairs, gates, a, b)
  pairs << [a, b]
  gates.each do |g|
    if g[:output] == a
      g[:output] = b
    elsif g[:output] == b
      g[:output] = a
    end
  end
end

def solution(gates)
  pairs = []
  num_z = gates.count { |g| g[:output].start_with?('z') }

  until pairs.size >= 4
    lookup, reverse_lookup = create_lookups(gates)
    adder = nil
    carry = nil

    num_z.times do |i|
      xi = "x%02d" % i
      yi = "y%02d" % i
      zi = "z%02d" % i

      if i == 0
        bit_key = get_reverse_lookup_key(xi, 'XOR', yi)
        adder = reverse_lookup[bit_key]

        carry_key = get_reverse_lookup_key(xi, 'AND', yi)
        carry = reverse_lookup[carry_key]
      else
        bit_key = get_reverse_lookup_key(xi, 'XOR', yi)
        bit = reverse_lookup[bit_key]

        if bit
          adder_key = get_reverse_lookup_key(bit, 'XOR', carry)
          adder = reverse_lookup[adder_key]

          if adder
            c1_key = get_reverse_lookup_key(xi, 'AND', yi)
            c1 = reverse_lookup[c1_key]

            c2_key = get_reverse_lookup_key(bit, 'AND', carry)
            c2 = reverse_lookup[c2_key]

            carry_key = get_reverse_lookup_key(c1, 'OR', c2)
            carry = reverse_lookup[carry_key]
          end
        end
      end

      if !adder
        gate = lookup[zi]
        unless gate.nil?
          bit_key = get_reverse_lookup_key(xi, 'XOR', yi)
          bit = reverse_lookup[bit_key]

          if bit
            gate_a_key = get_reverse_lookup_key(gate[:a], 'XOR', carry)
            if reverse_lookup.key?(gate_a_key)
              swap(pairs, gates, bit, gate[:a])
              break
            end

            gate_b_key = get_reverse_lookup_key(gate[:b], 'XOR', carry)
            if reverse_lookup.key?(gate_b_key)
              swap(pairs, gates, bit, gate[:b])
              break
            end
          end
        end
      elsif adder != zi
        swap(pairs, gates, adder, zi)
        break
      end
    end
  end

  result = pairs.flatten.sort.join(',')
  result
end

def main
  input = File.read('input.txt')
  parts = input.split("\n\n")
  return puts("Error parsing input") if parts.size != 2

  gates = []
  parts[1].split("\n").each do |line|
    next if line.empty?
    line_parts = line.split(' -> ')
    next if line_parts.size != 2

    gate_parts = line_parts[0].split(' ')
    next if gate_parts.size != 3

    gates << {
      gate: { a: gate_parts[0], op: gate_parts[1], b: gate_parts[2] },
      output: line_parts[1]
    }
  end

  puts(solution(gates))
end

main
