
import "io" for File

class Hand {
  construct new(cards, bid) {
    _cards = cards
    _bid = bid
    _type = Hand.getType(cards)
    _rank = Hand.getRank(cards)
  }
  cards { _cards }
  bid { _bid }
  type { _type }
  rank { _rank }

  static cardValue(c) {
    if (c == "T") return 10
    if (c == "J") return 11
    if (c == "Q") return 12
    if (c == "K") return 13
    if (c == "A") return 14
    return Num.fromString(c)
  }

  static getType(cards) {
    var counts = {}
    for (c in cards) {
      counts[c] = (counts[c] || 0) + 1
    }
    var pairs = 0
    var three = 0
    var four = 0
    var five = 0
    for (k in counts.keys) {
      var val = counts[k]
      if (val == 2) pairs = pairs + 1
      if (val == 3) three = three + 1
      if (val == 4) four = four + 1
      if (val == 5) five = five + 1
    }
    if (five > 0) return 7
    if (four > 0) return 6
    if (three > 0 && pairs > 0) return 5
    if (three > 0) return 4
    if (pairs == 2) return 3
    if (pairs == 1) return 2
    return 1
  }

  static getRank(cards) {
    var rank = 0
    for (c in cards) {
      rank = rank * 16 + Hand.cardValue(c)
    }
    return rank
  }

  static compare(a, b) {
    if (a.type != b.type) return a.type - b.type
    return a.rank - b.rank
  }
}

var content = File.read("input.txt")
var lines = content.split("\n")
var hands = []
for (line in lines) {
  var parts = []
  for (p in line.split(" ")) {
    var tp = p.trim()
    if (tp != "") parts.add(tp)
  }
  if (parts.count >= 2) {
    var cards = parts[0]
    var bid = Num.fromString(parts[1])
    hands.add(Hand.new(cards, bid))
  }
}

var n = hands.count
for (i in 1...n) {
  var key = hands[i]
  var j = i - 1
  while (j >= 0 && Hand.compare(hands[j], key) > 0) {
    hands[j + 1] = hands[j]
    j = j - 1
  }
  hands[j + 1] = key
}

var total = 0
for (i in 0...n) {
  total = total + hands[i].bid * (i + 1)
}
System.print(total)
