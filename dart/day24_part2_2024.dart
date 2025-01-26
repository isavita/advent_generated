
import 'dart:io';

class Gate {
  String a, op, b;
  Gate(this.a, this.op, this.b);
}

List<({Gate gate, String output})> parse(String input) {
  var parts = input.split('\n\n');
  if (parts.length != 2) return [];

  var gates = <({Gate gate, String output})>[];
  for (var line in parts[1].split('\n')) {
    if (line.isEmpty) continue;
    var lineParts = line.split(' -> ');
    if (lineParts.length != 2) continue;
    var gateParts = lineParts[0].split(' ');
    if (gateParts.length != 3) continue;
    gates.add((gate: Gate(gateParts[0], gateParts[1], gateParts[2]), output: lineParts[1]));
  }
  return gates;
}

({Map<String, Gate> lookup, Map<String, String> reverseLookup}) createLookups(List<({Gate gate, String output})> gates) {
  var lookup = <String, Gate>{};
  var reverseLookup = <String, String>{};

  for (var g in gates) {
    lookup[g.output] = g.gate;
    var inputs = [g.gate.a, g.gate.b]..sort();
    var key = '${inputs[0]}_${g.gate.op}_${inputs[1]}';
    reverseLookup[key] = g.output;
  }
  return (lookup: lookup, reverseLookup: reverseLookup);
}

void swap(List<List<String>> pairs, List<({Gate gate, String output})> gates, String a, String b) {
  pairs.add([a, b]);
  for (var i = 0; i < gates.length; i++) {
    if (gates[i].output == a) {
      gates[i] = (gate: gates[i].gate, output: b);
    } else if (gates[i].output == b) {
      gates[i] = (gate: gates[i].gate, output: a);
    }
  }
}

String getReverseLookupKey(String a, String op, String b) {
  var inputs = [a, b]..sort();
  return '${inputs[0]}_$op\_${inputs[1]}';
}

String solution(List<({Gate gate, String output})> gates) {
  var pairs = <List<String>>[];
  var numZ = gates.where((g) => g.output.startsWith('z')).length;

  while (pairs.length < 4) {
    String? adder, carry;
    var lookups = createLookups(gates);
    var lookup = lookups.lookup;
    var reverseLookup = lookups.reverseLookup;

    for (var i = 0; i < numZ; i++) {
      var xi = 'x${i.toString().padLeft(2, '0')}';
      var yi = 'y${i.toString().padLeft(2, '0')}';
      var zi = 'z${i.toString().padLeft(2, '0')}';

      if (i == 0) {
        adder = reverseLookup[getReverseLookupKey(xi, 'XOR', yi)];
        carry = reverseLookup[getReverseLookupKey(xi, 'AND', yi)];
      } else {
        var bit = reverseLookup[getReverseLookupKey(xi, 'XOR', yi)];
        if (bit != null) {
          adder = reverseLookup[getReverseLookupKey(bit, 'XOR', carry!)];
          if (adder != null) {
            var c1 = reverseLookup[getReverseLookupKey(xi, 'AND', yi)];
            var c2 = reverseLookup[getReverseLookupKey(bit, 'AND', carry)];
            carry = reverseLookup[getReverseLookupKey(c1!, 'OR', c2!)];
          }
        }
      }
      
      if(adder == null){
        var gate = lookup[zi]!;
        var bitKey = getReverseLookupKey(xi, 'XOR', yi);
        var bit = reverseLookup[bitKey];
        if(reverseLookup[getReverseLookupKey(gate.a, 'XOR', carry!)] != null){
          swap(pairs, gates, bit!, gate.a);
          break;
        } else if (reverseLookup[getReverseLookupKey(gate.b, 'XOR', carry!)] != null){
          swap(pairs, gates, bit!, gate.b);
          break;
        }
      } else if (adder != zi){
        swap(pairs, gates, adder, zi);
        break;
      }
    }
  }

  var result = <String>[];
  for (var pair in pairs) {
    result.addAll(pair);
  }
  result.sort();
  return result.join(',');
}

void main() {
  var input = File('input.txt').readAsStringSync();
  var gates = parse(input);
  print(solution(gates));
}
