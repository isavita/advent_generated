import Foundation

struct Hand {
    var cards: String
    var bid: Int
}

let fileURL = URL(fileURLWithPath: "input.txt")
let input = try String(contentsOf: fileURL)
let lines = input.components(separatedBy: "\n")

var hands = [Hand]()

let re = try NSRegularExpression(pattern: "[\\dAKQJT]+")
let bidRe = try NSRegularExpression(pattern: " [\\d]+")

for line in lines {
    if line.isEmpty {
        continue
    }

    let cardsRange = NSRange(location: 0, length: line.utf16.count)
    let cardsMatch = re.firstMatch(in: line, range: cardsRange)
    let bidMatch = bidRe.firstMatch(in: line, range: cardsRange)

    guard let cardsRange = cardsMatch?.range, let bidRange = bidMatch?.range else { continue }

    let cards = (line as NSString).substring(with: cardsRange)
    let bidString = (line as NSString).substring(with: bidRange).trimmingCharacters(in: .whitespaces)
    guard let bid = Int(bidString) else { continue }

    hands.append(Hand(cards: cards, bid: bid))
}

var matches = [[Hand](), [Hand](), [Hand](), [Hand](), [Hand](), [Hand](), [Hand]()]

let valueDict: [Character: Int] = ["J": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9, "T": 10, "Q": 11, "K": 12, "A": 13]

for hand in hands {
    var count = [Character: Int]()

    for card in hand.cards {
        count[card, default: 0] += 1
    }

    if let jCount = count["J"], jCount > 0 {
        var highV = 0
        var highKey: Character = "J"
        for (key, value) in count {
            if key != "J" {
                if value > highV {
                    highKey = key
                    highV = value
                } else if value == highV && valueDict[key]! > valueDict[highKey]! {
                    highKey = key
                }
            }
        }
        if highKey != "J" {
            count[highKey, default: 0] += jCount
            count["J"] = nil
        }
    }

    var value = 1
    for (_, i) in count {
        value *= i
    }

    switch value {
    case 1:
        matches[6].append(hand)
    case 2:
        matches[5].append(hand)
    case 3:
        matches[3].append(hand)
    case 4:
        if count.count == 2 {
            matches[1].append(hand)
        } else {
            matches[4].append(hand)
        }
    case 5:
        matches[0].append(hand)
    case 6:
        matches[2].append(hand)
    default:
        print("oh no")
    }
}

var convertedMatches = [[Int]]()

for x in matches {
    var temp = [[Int]]()
    for i in x {
        var y = i.cards.replacingOccurrences(of: "A", with: "E")
        y = y.replacingOccurrences(of: "T", with: "A")
        y = y.replacingOccurrences(of: "J", with: "1")
        y = y.replacingOccurrences(of: "Q", with: "C")
        y = y.replacingOccurrences(of: "K", with: "D")
        if let val = Int(y, radix: 16) {
            temp.append([val, i.bid])
        }
    }
    temp.sort { $0[0] > $1[0] }
    convertedMatches.append(contentsOf: temp)
}

var total = 0
for x in 0..<convertedMatches.count {
    total += convertedMatches[x][1] * (convertedMatches.count - x)
}

print(total)