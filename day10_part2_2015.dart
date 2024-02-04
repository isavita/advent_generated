
import 'dart:io';

void main() {
  var initialSequence = File('input.txt').readAsStringSync().trim();
  var result = lookAndSay(initialSequence, 50);
  print(result.length);
}

String lookAndSay(String sequence, int iterations) {
  for (var i = 0; i < iterations; i++) {
    sequence = nextSequence(sequence);
  }
  return sequence;
}

String nextSequence(String sequence) {
  var result = StringBuffer();
  for (var i = 0; i < sequence.length;) {
    var count = 1;
    var digit = sequence[i];
    for (var j = i + 1; j < sequence.length && sequence[j] == digit; j++) {
      count++;
    }
    result.write('$count$digit');
    i += count;
  }
  return result.toString();
}
