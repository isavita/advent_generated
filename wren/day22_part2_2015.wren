
import "io" for File

class Game {
    static init() {
        __minMana = 999999
        __bossDamage = 0
    }
    static minMana { __minMana }
    static bossDamage=(v) { __bossDamage = v }

    static simulate(pHP, pM, bHP, sh, po, re, spent, turn) {
        if (spent >= __minMana) return

        if (turn == 1) {
            pHP = pHP - 1
            if (pHP <= 0) return
        }

        if (sh > 0) sh = sh - 1
        if (po > 0) {
            bHP = bHP - 3
            po = po - 1
        }
        if (re > 0) {
            pM = pM + 101
            re = re - 1
        }

        if (bHP <= 0) {
            if (spent < __minMana) __minMana = spent
            return
        }

        if (pHP <= 0) return

        if (turn == 0) {
            var dmg = __bossDamage
            if (sh > 0) dmg = dmg - 7
            if (dmg < 1) dmg = 1
            pHP = pHP - dmg
            simulate(pHP, pM, bHP, sh, po, re, spent, 1)
            return
        }

        if (pM >= 53) {
            simulate(pHP, pM - 53, bHP - 4, sh, po, re, spent + 53, 0)
        }
        if (pM >= 73) {
            simulate(pHP + 2, pM - 73, bHP - 2, sh, po, re, spent + 73, 0)
        }
        if (pM >= 113 && sh == 0) {
            simulate(pHP, pM - 113, bHP, 6, po, re, spent + 113, 0)
        }
        if (pM >= 173 && po == 0) {
            simulate(pHP, pM - 173, bHP, sh, 6, re, spent + 173, 0)
        }
        if (pM >= 229 && re == 0) {
            simulate(pHP, pM - 229, bHP, sh, po, 5, spent + 229, 0)
        }
    }
}

var content = File.read("input.txt").replace("\r", "")
var bossHP = 0
var bossDamage = 0

for (line in content.split("\n")) {
    var parts = line.trim().split(" ")
    if (parts.count >= 3 && parts[0] == "Hit") {
        bossHP = Num.fromString(parts[2])
    } else if (parts.count >= 2 && parts[0] == "Damage:") {
        bossDamage = Num.fromString(parts[1])
    }
}

Game.init()
Game.bossDamage = bossDamage
Game.simulate(50, 500, bossHP, 0, 0, 0, 0, 1)
System.print(Game.minMana)
