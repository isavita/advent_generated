import 'dart:io';

String filterValues(List<String> values, String Function(int zeros, int ones) criteria) {
  for (int i = 0; i < values[0].length; i++) {
    int zeros = 0, ones = 0;
    for (String val in values) {
      if (val[i] == '0') {
        zeros++;
      } else {
        ones++;
      }
    }
    String keep = criteria(zeros, ones);
    values = filterByBit(values, i, keep);
    if (values.length == 1) {
      break;
    }
  }
  return values[0];
}

List<String> filterByBit(List<String> values, int bitIndex, String keep) {
  List<String> filtered = [];
  for (String val in values) {
    if (val[bitIndex] == keep) {
      filtered.add(val);
    }
  }
  return filtered;
}

void main() {
  File file = File('input.txt');
  List<String> values = file.readAsLinesSync();

  String oxygenGeneratorRating = filterValues(values, (int zeros, int ones) {
    if (zeros > ones) {
      return '0';
    } else {
      return '1';
    }
  });
  int oxygenGeneratorRatingInt = int.parse(oxygenGeneratorRating, radix: 2);

  String co2ScrubberRating = filterValues(values, (int zeros, int ones) {
    if (zeros <= ones) {
      return '0';
    } else {
      return '1';
    }
  });
  int co2ScrubberRatingInt = int.parse(co2ScrubberRating, radix: 2);

  print(oxygenGeneratorRatingInt * co2ScrubberRatingInt);
}