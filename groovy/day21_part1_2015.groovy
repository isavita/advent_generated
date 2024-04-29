def input = new File('input.txt').text.split('\n')
def bossHp = input[0].tokenize()[2].toInteger()
def bossDamage = input[1].tokenize()[1].toInteger()
def bossArmor = input[2].tokenize()[1].toInteger()

def weapons = [
    [8, 4, 0], [10, 5, 0], [25, 6, 0], [40, 7, 0], [74, 8, 0]
]
def armors = [
    [0, 0, 0], [13, 0, 1], [31, 0, 2], [53, 0, 3], [75, 0, 4], [102, 0, 5]
]
def rings = [
    [0, 0, 0], [25, 1, 0], [50, 2, 0], [100, 3, 0], [20, 0, 1], [40, 0, 2], [80, 0, 3]
]

def minCost = Integer.MAX_VALUE
for (weapon in weapons) {
    for (armor in armors) {
        for (int i = 0; i < rings.size(); i++) {
            for (int j = i; j < rings.size(); j++) {
                def cost = weapon[0] + armor[0] + rings[i][0] + rings[j][0]
                def playerDamage = weapon[1] + armor[1] + rings[i][1] + rings[j][1]
                def playerArmor = weapon[2] + armor[2] + rings[i][2] + rings[j][2]
                def playerHp = 100
                def bossHpLeft = bossHp
                while (playerHp > 0 && bossHpLeft > 0) {
                    bossHpLeft -= Math.max(1, playerDamage - bossArmor)
                    if (bossHpLeft <= 0) break
                    playerHp -= Math.max(1, bossDamage - playerArmor)
                }
                if (playerHp > 0) {
                    minCost = Math.min(minCost, cost)
                }
            }
        }
    }
}

println minCost