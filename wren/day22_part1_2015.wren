
import "io" for File

class Spell {
    construct new(name, cost, damage, heal, duration, effect) {
        _name = name
        _cost = cost
        _damage = damage
        _heal = heal
        _duration = duration
        _effect = effect
    }
    name { _name }
    cost { _cost }
    damage { _damage }
    heal { _heal }
    duration { _duration }
    effect { _effect }
}

class Solver {
    construct new(bossHp, bossDamage) {
        _bossHp = bossHp
        _bossDamage = bossDamage
        _minManaSpent = 999999
        _spells = [
            Spell.new("MM", 53, 4, 0, 0, null),
            Spell.new("D", 73, 2, 2, 0, null),
            Spell.new("S", 113, 0, 0, 6, "Shield"),
            Spell.new("P", 173, 0, 0, 6, "Poison"),
            Spell.new("R", 229, 0, 0, 5, "Recharge")
        ]
    }

    minManaSpent { _minManaSpent }

    solve() {
        simulate(50, 500, _bossHp, 0, 0, 0, 0, true)
    }

    simulate(pHp, pMana, bHp, sTimer, pTimer, rTimer, manaSpent, isPlayerTurn) {
        if (manaSpent >= _minManaSpent) return

        var pArmor = 0
        if (sTimer > 0) pArmor = 7
        if (pTimer > 0) bHp = bHp - 3
        if (rTimer > 0) pMana = pMana + 101

        if (bHp <= 0) {
            if (manaSpent < _minManaSpent) _minManaSpent = manaSpent
            return
        }

        var nextSTimer = sTimer > 0 ? sTimer - 1 : 0
        var nextPTimer = pTimer > 0 ? pTimer - 1 : 0
        var nextRTimer = rTimer > 0 ? rTimer - 1 : 0

        if (isPlayerTurn) {
            for (spell in _spells) {
                if (pMana >= spell.cost) {
                    var canCast = true
                    if (spell.effect == "Shield" && nextSTimer > 0) canCast = false
                    if (spell.effect == "Poison" && nextPTimer > 0) canCast = false
                    if (spell.effect == "Recharge" && nextRTimer > 0) canCast = false

                    if (canCast) {
                        var nextPHp = pHp
                        var nextPMana = pMana - spell.cost
                        var nextBHp = bHp - spell.damage
                        nextPHp = nextPHp + spell.heal
                        var nextManaSpent = manaSpent + spell.cost

                        var spellSTimer = nextSTimer
                        var spellPTimer = nextPTimer
                        var spellRTimer = nextRTimer

                        if (spell.effect == "Shield") spellSTimer = spell.duration
                        if (spell.effect == "Poison") spellPTimer = spell.duration
                        if (spell.effect == "Recharge") spellRTimer = spell.duration

                        if (nextBHp <= 0) {
                            if (nextManaSpent < _minManaSpent) _minManaSpent = nextManaSpent
                        } else {
                            simulate(nextPHp, nextPMana, nextBHp, spellSTimer, spellPTimer, spellRTimer, nextManaSpent, false)
                        }
                    }
                }
            }
        } else {
            var damage = _bossDamage - pArmor
            if (damage < 1) damage = 1
            var nextPHp = pHp - damage
            if (nextPHp > 0) {
                simulate(nextPHp, pMana, bHp, nextSTimer, nextPTimer, nextRTimer, manaSpent, true)
            }
        }
    }
}

var lines = File.read("input.txt").split("\n")
var bossHp = 0
var bossDamage = 0
for (line in lines) {
    if (line.contains("Hit Points:")) {
        bossHp = Num.fromString(line.split(":")[1].trim())
    } else if (line.contains("Damage:")) {
        bossDamage = Num.fromString(line.split(":")[1].trim())
    }
}

var solver = Solver.new(bossHp, bossDamage)
solver.solve()
System.print(solver.minManaSpent)
