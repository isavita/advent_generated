import 'dart:io';
import 'dart:convert';
import 'dart:math';

class Hand {
  String cards;
  int bid;

  Hand(this.cards, this.bid);
}

final valueDict = {'J': 1, '2': 2, '3': 3, '4': 4, '5': 5, '6': 6, '7': 7, '8': 8, '9': 9, 'T': 10, 'Q': 11, 'K': 12, 'A': 13};

void main() {
  final file = File('input.txt');
  final input = file.readAsStringSync();
  final lines = input.split('\n');

  final hands = <Hand>[];

  for (final line in lines) {
    if (line.isEmpty) continue;
    final cardMatch = RegExp(r'[\dAKQJT]+').firstMatch(line);
    final cards = cardMatch?.group(0) ?? '';
    final bidMatch = RegExp(r' [\d]+').firstMatch(line);
    final bid = int.parse(bidMatch?.group(0)?.substring(1) ?? '0');
    hands.add(Hand(cards, bid));
  }

  final matches = [[], [], [], [], [], [], []];

  for (final hand in hands) {
    final count = <String, int>{};
    for (final char in hand.cards.split('')) {
      count[char] = (count[char] ?? 0) + 1;
    }

    if (count['J'] != null && count['J']! > 0) {
      int highV = 0;
      String highKey = 'J';
      for (final key in count.keys) {
        if (key != 'J') {
          if (count[key]! > highV) {
            highKey = key;
            highV = count[key]!;
          } else if (count[key]! == highV && valueDict[key]! > valueDict[highKey]!) {
            highKey = key;
          }
        }
      }
      if (highKey != 'J') {
        count[highKey] = (count[highKey] ?? 0) + count['J']!;
        count.remove('J');
      }
    }

    int value = 1;
    for (final i in count.values) {
      value *= i;
    }

    switch (value) {
      case 1:
        matches[6].add(hand);
        break;
      case 2:
        matches[5].add(hand);
        break;
      case 3:
        matches[3].add(hand);
        break;
      case 4:
        if (count.length == 2) {
          matches[1].add(hand);
        } else {
          matches[4].add(hand);
        }
        break;
      case 5:
        matches[0].add(hand);
        break;
      case 6:
        matches[2].add(hand);
        break;
      default:
        print('oh no');
    }
  }

  final convertedMatches = <List<int>>[];

  for (final x in matches) {
    final temp = <List<int>>[];
    for (final i in x) {
      var y = i.cards.replaceAll('A', 'E');
      y = y.replaceAll('T', 'A');
      y = y.replaceAll('J', '1');
      y = y.replaceAll('Q', 'C');
      y = y.replaceAll('K', 'D');
      final val = int.parse(y, radix: 16);
      temp.add([val, i.bid]);
    }
    temp.sort((a, b) => b[0].compareTo(a[0]));
    convertedMatches.addAll(temp);
  }

  int total = 0;
  for (var x = 0; x < convertedMatches.length; x++) {
    total += convertedMatches[x][1] * (convertedMatches.length - x);
  }

  print(total);
}