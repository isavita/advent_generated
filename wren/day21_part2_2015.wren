
import "io" for File
import "os" for Process

class Item {
    construct new(cost, damage, armor) {
        _cost = cost
        _damage = damage
        _armor = armor
    }
    
    cost { _cost }
    damage { _damage }
    armor { _armor }
}

class Character {
    construct new(hitPoints, damage, armor) {
        _hitPoints = hitPoints
        _damage = damage
        _armor = armor
    }
    
    hitPoints { _hitPoints }
    damage { _damage }
    armor { _armor }
}

class Solution {
    static solve() {
        var input = File.read("input.txt").split("\n")
        var boss = Character.new(
            Num.fromString(input[0].split(": ")[1]),
            Num.fromString(input[1].split(": ")[1]),
            Num.fromString(input[2].split(": ")[1])
        )

        var weapons = [
            Item.new(8, 4, 0),
            Item.new(10, 5, 0),
            Item.new(25, 6, 0),
            Item.new(40, 7, 0),
            Item.new(74, 8, 0)
        ]

        var armors = [
            Item.new(0, 0, 0),
            Item.new(13, 0, 1),
            Item.new(31, 0, 2),
            Item.new(53, 0, 3),
            Item.new(75, 0, 4),
            Item.new(102, 0, 5)
        ]

        var rings = [
            Item.new(0, 0, 0),
            Item.new(25, 1, 0),
            Item.new(50, 2, 0),
            Item.new(100, 3, 0),
            Item.new(20, 0, 1),
            Item.new(40, 0, 2),
            Item.new(80, 0, 3)
        ]

        var maxCost = 0
        for (w in weapons) {
            for (a in armors) {
                for (i in 0...rings.count) {
                    for (j in (i+1)...rings.count) {
                        var player = Character.new(100, 
                            w.damage + rings[i].damage + rings[j].damage, 
                            a.armor + rings[i].armor + rings[j].armor
                        )
                        
                        var cost = w.cost + a.cost + rings[i].cost + rings[j].cost
                        
                        if (!playerWins(player, boss) && cost > maxCost) {
                            maxCost = cost
                        }
                    }
                }
            }
        }

        System.print(maxCost)
    }

    static playerWins(player, boss) {
        var playerDamage = (player.damage - boss.armor).max(1)
        var bossDamage = (boss.damage - player.armor).max(1)

        var playerTurns = ((boss.hitPoints + playerDamage - 1) / playerDamage).floor
        var bossTurns = ((player.hitPoints + bossDamage - 1) / bossDamage).floor

        return playerTurns <= bossTurns
    }
}

Solution.solve()
