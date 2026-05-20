
import "io" for File

class Hand {
  construct new(cards, bid, id) {
    _cards = cards
    _bid = bid
    _id = id
    _type = Hand.classify(cards)
    _hexVal = Hand.toHexVal(cards)
  }
  cards { _cards }
  bid { _bid }
  id { _id }
  type { _type }
  hexVal { _hexVal }

  static cardValues {
    if (!__cardValues) {
      __cardValues = {
        "J": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9, "T": 10, "Q": 11, "K": 12, "A": 13
      }
    }
    return __cardValues
  }

  static hexMap {
    if (!__hexMap) {
      __hexMap = {
        "A": 14, "K": 13, "Q": 12, "J": 1, "T": 10,
        "9": 9, "8": 8, "7": 7, "6": 6, "5": 5, "4": 4, "3": 3, "2": 2
      }
    }
    return __hexMap
  }

  static toHexVal(cards) {
    var val = 0
    for (i in 0...cards.count) {
      val = val * 16 + hexMap[cards[i]]
    }
    return val
  }

  static classify(cards) {
    var counts = {}
    for (i in 0...cards.count) {
      var c = cards[i]
      counts[c] = (counts[c] || 0) + 1
    }

    if (counts.containsKey("J") && counts["J"] > 0) {
      var jokerCount = counts["J"]
      counts.remove("J")
      if (counts.isEmpty) {
        counts["J"] = jokerCount
      } else {
        var highKey = ""
        var highV = -1
        for (card in counts.keys) {
          var v = counts[card]
          if (v > highV) {
            highKey = card
            highV = v
          } else if (v == highV) {
            if (cardValues[card] > cardValues[highKey]) {
              highKey = card
            }
          }
        }
        counts[highKey] = counts[highKey] + jokerCount
      }
    }

    var valueProduct = 1
    var distinct = 0
    for (k in counts.keys) {
      distinct = distinct + 1
      valueProduct = valueProduct * counts[k]
    }

    if (valueProduct == 1 && distinct == 5) return 6
    if (valueProduct == 2 && distinct == 4) return 5
    if (valueProduct == 3 && distinct == 3) return 3
    if (valueProduct == 4) {
      if (distinct == 2) return 1
      return 4
    }
    if (valueProduct == 5 && distinct == 1) return 0
    if (valueProduct == 6 && distinct == 2) return 2
    return -1
  }

  static compare(a, b) {
    if (a.type != b.type) return a.type < b.type ? -1 : 1
    if (a.hexVal != b.hexVal) return a.hexVal > b.hexVal ? -1 : 1
    return a.id < b.id ? -1 : 1
  }
}

class Sort {
  static quicksort(list, low, high) {
    if (low < high) {
      var p = partition(list, low, high)
      quicksort(list, low, p - 1)
      quicksort(list, p + 1, high)
    }
  }

  static partition(list, low, high) {
    var pivot = list[high]
    var i = low - 1
    for (j in low...high) {
      if (Hand.compare(list[j], pivot) <= 0) {
        i = i + 1
        var temp = list[i]
        list[i] = list[j]
        list[j] = temp
      }
    }
    var temp = list[i + 1]
    list[i + 1] = list[high]
    list[high] = temp
    return i + 1
  }
}

var content = File.read("input.txt")
var lines = content.split("\n")
var hands = []
var id = 0
for (line in lines) {
  line = line.replace("\t", " ").trim()
  if (line != "") {
    var parts = line.split(" ")
    var cleanParts = []
    for (p in parts) {
      if (p != "") cleanParts.add(p)
    }
    if (cleanParts.count >= 2) {
      id = id + 1
      hands.add(Hand.new(cleanParts[0], Num.fromString(cleanParts[1]), id))
    }
  }
}

if (hands.count == 0) {
  System.print(0)
} else {
  Sort.quicksort(hands, 0, hands.count - 1)
  var total = 0
  var N = hands.count
  for (i in 0...N) {
    total = total + hands[i].bid * (N - i)
  }
  System.print(total)
}
