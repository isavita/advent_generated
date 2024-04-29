import groovy.io.FileType

class Item {
    int cost
    int damage
    int armor
}

class Character {
    int hitPoints
    int damage
    int armor
}

def file = new File('input.txt')
def lines = file.text.split('\n')
def boss = new Character(hitPoints: lines[0].split(': ')[1].toInteger(), 
                        damage: lines[1].split(': ')[1].toInteger(), 
                        armor: lines[2].split(': ')[1].toInteger())

def weapons = [
    new Item(cost: 8, damage: 4),
    new Item(cost: 10, damage: 5),
    new Item(cost: 25, damage: 6),
    new Item(cost: 40, damage: 7),
    new Item(cost: 74, damage: 8)
]

def armors = [
    new Item(cost: 0, armor: 0), 
    new Item(cost: 13, armor: 1),
    new Item(cost: 31, armor: 2),
    new Item(cost: 53, armor: 3),
    new Item(cost: 75, armor: 4),
    new Item(cost: 102, armor: 5)
]

def rings = [
    new Item(cost: 0), 
    new Item(cost: 25, damage: 1),
    new Item(cost: 50, damage: 2),
    new Item(cost: 100, damage: 3),
    new Item(cost: 20, armor: 1),
    new Item(cost: 40, armor: 2),
    new Item(cost: 80, armor: 3)
]

def maxCost = 0
weapons.each { w ->
    armors.each { a ->
        for (int ri = 0; ri < rings.size(); ri++) {
            for (int rj = ri + 1; rj < rings.size(); rj++) {
                def player = new Character(hitPoints: 100, damage: w.damage, armor: a.armor)
                player.damage += rings[ri].damage + rings[rj].damage
                player.armor += rings[ri].armor + rings[rj].armor
                def cost = w.cost + a.cost + rings[ri].cost + rings[rj].cost
                if (!playerWins(player, boss) && cost > maxCost) {
                    maxCost = cost
                }
            }
        }
    }
}

println maxCost

def max(a, b) {
    a > b ? a : b
}

def playerWins(player, boss) {
    def playerDamage = Math.max(1, player.damage - boss.armor)
    def bossDamage = Math.max(1, boss.damage - player.armor)

    def playerTurns = (boss.hitPoints + playerDamage - 1) / playerDamage
    def bossTurns = (player.hitPoints + bossDamage - 1) / bossDamage

    playerTurns <= bossTurns
}