
import Foundation

struct Item {
    var cost: Int
    var damage: Int
    var armor: Int
}

struct Character {
    var hitPoints: Int
    var damage: Int
    var armor: Int
}

func playerWins(player: Character, boss: Character) -> Bool {
    let playerDamage = max(1, player.damage - boss.armor)
    let bossDamage = max(1, boss.damage - player.armor)
    
    let playerTurns = (boss.hitPoints + playerDamage - 1) / playerDamage
    let bossTurns = (player.hitPoints + bossDamage - 1) / bossDamage
    
    return playerTurns <= bossTurns
}

func max(_ a: Int, _ b: Int) -> Int {
    return a > b ? a : b
}

let fileURL = URL(fileURLWithPath: "input.txt")
let data = try String(contentsOf: fileURL, encoding: .utf8)
let lines = data.components(separatedBy: "\n")

let boss = Character(hitPoints: Int(lines[0].components(separatedBy: ": ")[1])!,
                     damage: Int(lines[1].components(separatedBy: ": ")[1])!,
                     armor: Int(lines[2].components(separatedBy: ": ")[1])!)

let weapons = [
    Item(cost: 8, damage: 4, armor: 0),
    Item(cost: 10, damage: 5, armor: 0),
    Item(cost: 25, damage: 6, armor: 0),
    Item(cost: 40, damage: 7, armor: 0),
    Item(cost: 74, damage: 8, armor: 0)
]

let armors = [
    Item(cost: 0, damage: 0, armor: 0),
    Item(cost: 13, damage: 0, armor: 1),
    Item(cost: 31, damage: 0, armor: 2),
    Item(cost: 53, damage: 0, armor: 3),
    Item(cost: 75, damage: 0, armor: 4),
    Item(cost: 102, damage: 0, armor: 5)
]

let rings = [
    Item(cost: 0, damage: 0, armor: 0),
    Item(cost: 25, damage: 1, armor: 0),
    Item(cost: 50, damage: 2, armor: 0),
    Item(cost: 100, damage: 3, armor: 0),
    Item(cost: 20, damage: 0, armor: 1),
    Item(cost: 40, damage: 0, armor: 2),
    Item(cost: 80, damage: 0, armor: 3)
]

var maxCost = 0
for w in weapons {
    for a in armors {
        for ri in 0..<rings.count {
            for rj in ri+1..<rings.count {
                var player = Character(hitPoints: 100, damage: w.damage, armor: a.armor)
                player.damage += rings[ri].damage + rings[rj].damage
                player.armor += rings[ri].armor + rings[rj].armor
                let cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost
                if !playerWins(player: player, boss: boss) && cost > maxCost {
                    maxCost = cost
                }
            }
        }
    }
}

print(maxCost)
