import 'dart:io';
import 'dart:math';

const highCard = 1;
const onePair = 2;
const twoPair = 3;
const threeKind = 4;
const fullHouse = 5;
const fourKind = 6;
const fiveKind = 7;

class Hand {
  String cards;
  int bid;

  Hand(this.cards, this.bid);
}

class RankedHand {
  Hand hand;
  int rank;

  RankedHand(this.hand, this.rank);
}

List<List<Hand>> matches = [[], [], [], [], [], [], []];

void findMatches(List<Hand> hands) {
  for (var hand in hands) {
    var count = <String, int>{};

    for (var card in hand.cards.runes) {
      count[String.fromCharCode(card)] = (count[String.fromCharCode(card)] ?? 0) + 1;
    }

    var value = 1;
    count.values.forEach((c) {
      value *= c;
    });

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
}

List<RankedHand> convertAndOrderMatches() {
  var convertedMatches = <RankedHand>[];

  for (var category in matches) {
    var temp = <RankedHand>[];

    for (var hand in category) {
      var cards = hand.cards.replaceAll('A', 'E')
          .replaceAll('T', 'A')
          .replaceAll('J', 'B')
          .replaceAll('Q', 'C')
          .replaceAll('K', 'D');

      var num = int.parse(cards, radix: 16);

      temp.add(RankedHand(hand, num));
    }

    temp.sort((a, b) => b.rank.compareTo(a.rank));

    convertedMatches.addAll(temp);
  }

  return convertedMatches;
}

void main() {
  var lines = File('input.txt').readAsStringSync();
  var hands = <Hand>[];

  var re = RegExp(r'[\dAKQJT]+');
  var bidRe = RegExp(r' [\d]+');

  for (var line in lines.split('\n')) {
    if (line.isEmpty) {
      continue;
    }

    var cards = re.firstMatch(line)!.group(0)!;
    var bid = int.parse(bidRe.firstMatch(line)!.group(0)!.substring(1));

    hands.add(Hand(cards, bid));
  }

  findMatches(hands);

  var convertedMatches = convertAndOrderMatches();

  var total = 0;
  for (var i = 0; i < convertedMatches.length; i++) {
    total += convertedMatches[i].hand.bid * (convertedMatches.length - i);
  }

  print(total);
}