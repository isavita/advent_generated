
import Foundation

enum HandRank: Int {
    case highCard = 1, onePair, twoPair, threeKind, fullHouse, fourKind, fiveKind
}

struct Hand {
    let cards: String
    let bid: Int
}

struct RankedHand {
    let hand: Hand
    let rank: Int
}

var matches = [[Hand]](repeating: [], count: 7)

func findMatches(_ hands: [Hand]) {
    for hand in hands {
        var count = [Character: Int]()
        
        for card in hand.cards {
            count[card, default: 0] += 1
        }
        
        let value = count.values.reduce(1, { $0 * $1 })
        
        switch value {
        case 1: matches[6].append(hand)
        case 2: matches[5].append(hand)
        case 3: matches[3].append(hand)
        case 4: matches[count.count == 2 ? 1 : 4].append(hand)
        case 5: matches[0].append(hand)
        case 6: matches[2].append(hand)
        default: print("oh no")
        }
    }
}

func convertAndOrderMatches() -> [RankedHand] {
    var convertedMatches = [RankedHand]()
    
    for category in matches {
        var temp = [RankedHand]()
        
        for hand in category {
            let cards = hand.cards
                .replacingOccurrences(of: "A", with: "E")
                .replacingOccurrences(of: "T", with: "A")
                .replacingOccurrences(of: "J", with: "B")
                .replacingOccurrences(of: "Q", with: "C")
                .replacingOccurrences(of: "K", with: "D")
            
            if let num = Int(cards, radix: 16) {
                temp.append(RankedHand(hand: hand, rank: num))
            }
        }
        
        temp.sort { $0.rank > $1.rank }
        convertedMatches.append(contentsOf: temp)
    }
    
    return convertedMatches
}

func main() {
    let url = URL(fileURLWithPath: "input.txt")
    guard let lines = try? String(contentsOf: url).split(separator: "\n").map(String.init) else { return }
    
    var hands = [Hand]()
    
    for line in lines {
        guard !line.isEmpty else { continue }
        
        let components = line.split(separator: " ")
        let cards = String(components[0])
        let bid = Int(components[1]) ?? 0
        
        hands.append(Hand(cards: cards, bid: bid))
    }
    
    findMatches(hands)
    let convertedMatches = convertAndOrderMatches()
    
    let total = convertedMatches.enumerated().reduce(0) { $0 + $1.element.hand.bid * (convertedMatches.count - $1.offset) }
    
    print(total)
}

main()
