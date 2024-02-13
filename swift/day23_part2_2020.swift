
import Foundation

class Cup {
    var value: Int
    var next: Cup?

    init(value: Int) {
        self.value = value
    }
}

func playCrabCups(input: String, totalCups: Int, moves: Int) -> Int {
    var cupsMap = [Int: Cup]()
    var currentCup: Cup?
    var firstCup: Cup?

    for cupValue in input.compactMap({ Int(String($0)) }) {
        let cup = Cup(value: cupValue)
        cupsMap[cupValue] = cup

        if currentCup == nil {
            currentCup = cup
            firstCup = cup
        } else {
            currentCup?.next = cup
            currentCup = cup
        }
    }

    for cupValue in (input.count + 1)...totalCups {
        let cup = Cup(value: cupValue)
        cupsMap[cupValue] = cup

        currentCup?.next = cup
        currentCup = cup
    }

    currentCup?.next = firstCup

    currentCup = firstCup

    for _ in 0..<moves {
        let pickedUpCups = [currentCup!.next!, currentCup!.next!.next!, currentCup!.next!.next!.next!]

        currentCup!.next = pickedUpCups.last!.next

        var destinationValue = currentCup!.value - 1
        while pickedUpCups.contains(where: { $0.value == destinationValue }) || destinationValue < 1 {
            if destinationValue < 1 {
                destinationValue = totalCups
            } else {
                destinationValue -= 1
            }
        }

        let destinationCup = cupsMap[destinationValue]!

        pickedUpCups.last!.next = destinationCup.next
        destinationCup.next = pickedUpCups.first

        currentCup = currentCup?.next
    }

    let cup1 = cupsMap[1]!
    return cup1.next!.value * cup1.next!.next!.value
}

if let input = try? String(contentsOfFile: "input.txt") {
    let result = playCrabCups(input: input, totalCups: 1000000, moves: 10000000)
    print(result)
}
