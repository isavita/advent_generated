
BEGIN {
    FS = ": "
    totalCost = 99999
}

{
    if (NR == 1) {
        bossHitPoints = $2
    } else if (NR == 2) {
        bossDamage = $2
    } else if (NR == 3) {
        bossArmor = $2
    }
}

END {
    weapons[1] = "8:4"
    weapons[2] = "10:5"
    weapons[3] = "25:6"
    weapons[4] = "40:7"
    weapons[5] = "74:8"

    armors[1] = "0:0"
    armors[2] = "13:1"
    armors[3] = "31:2"
    armors[4] = "53:3"
    armors[5] = "75:4"
    armors[6] = "102:5"

    rings[1] = "0:0:0"
    rings[2] = "25:1:0"
    rings[3] = "50:2:0"
    rings[4] = "100:3:0"
    rings[5] = "20:0:1"
    rings[6] = "40:0:2"
    rings[7] = "80:0:3"

    for (w = 1; w <= 5; w++) {
        split(weapons[w], weapon, ":")
        for (a = 1; a <= 6; a++) {
            split(armors[a], armor, ":")
            for (ri = 1; ri <= 7; ri++) {
                split(rings[ri], ringi, ":")
                for (rj = ri + 1; rj <= 7; rj++) {
                    split(rings[rj], ringj, ":")
                    playerHitPoints = 100
                    playerDamage = weapon[2] + ringi[2] + ringj[2]
                    playerArmor = armor[2] + ringi[3] + ringj[3]
                    cost = weapon[1] + armor[1] + ringi[1] + ringj[1]
                    if (playerWins(playerHitPoints, playerDamage, playerArmor, bossHitPoints, bossDamage, bossArmor) && cost < totalCost) {
                        totalCost = cost
                    }
                }
            }
        }
    }

    print totalCost
}

function playerWins(playerHitPoints, playerDamage, playerArmor, bossHitPoints, bossDamage, bossArmor) {
    playerDamage = playerDamage - bossArmor > 0 ? playerDamage - bossArmor : 1
    bossDamage = bossDamage - playerArmor > 0 ? bossDamage - playerArmor : 1

    playerTurns = int((bossHitPoints + playerDamage - 1) / playerDamage)
    bossTurns = int((playerHitPoints + bossDamage - 1) / bossDamage)
    
    return playerTurns <= bossTurns
}
