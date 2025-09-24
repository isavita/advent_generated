import java.nio.file.Files
import java.nio.file.Paths

class Group {
    int units
    int hitPoints
    int attackDamage
    String attackType
    int initiative
    List<String> immunities = []
    List<String> weaknesses = []
    Group attacker = null
    Group target = null
    int armyId = 0
    int effectivePower(){ units * attackDamage }
    int damageTo(Group enemy){
        if(enemy.immunities.contains(attackType)) return 0
        int d = effectivePower()
        if(enemy.weaknesses.contains(attackType)) d *= 2
        return d
    }
}

class Army {
    List<Group> groups = []
    boolean alive(){ groups.any{ it.units>0 } }
    void boost(int amount){ groups.each{ it.attackDamage += amount } }
    void sortForTargeting(){ groups.sort{ a,b -> b.effectivePower() <=> a.effectivePower() ?: b.initiative <=> a.initiative } }
}

def lines = Files.readAllLines(Paths.get('input.txt')).collect{ it.trim() }.findAll{ it }
def templates = []
int currentArmy = 0
lines.each{ line ->
    if(line.endsWith(':')){
        currentArmy = line.startsWith('Immune System') ? 1 : 2
    } else {
        def m = (line =~ /(\d+) units each with (\d+) hit points(.*) with an attack that does (\d+) (\w+) damage at initiative (\d+)/)
        if(m){
            def units = m[0][1].toInteger()
            def hp = m[0][2].toInteger()
            def mid = m[0][3]
            def atk = m[0][4].toInteger()
            def atktype = m[0][5]
            def init = m[0][6].toInteger()
            def im = []
            def wk = []
            def mi = (mid =~ /immune to ([^;)\.]+)/)
            if(mi){ im = mi[0][1].split(/,\s*/).toList() }
            def mw = (mid =~ /weak to ([^;)\.]+)/)
            if(mw){ wk = mw[0][1].split(/,\s*/).toList() }
            templates << [army:currentArmy, units:units, hp:hp, atk:atk, type:atktype, init:init, imm:im, wk:wk]
        }
    }
}

def cloneBattle = { ->
    def battle = [(1): new Army(), (2): new Army()]
    templates.each{ t ->
        def g = new Group()
        g.units = t.units
        g.hitPoints = t.hp
        g.attackDamage = t.atk
        g.attackType = t.type
        g.initiative = t.init
        g.immunities = t.imm as List
        g.weaknesses = t.wk as List
        g.armyId = t.army
        battle[t.army].groups << g
    }
    return battle
}

def totalUnits = { battle ->
    battle.values().collectMany{ it.groups }.sum{ g -> g.units>0 ? g.units : 0 } as int
}

def findTargets = { battle ->
    battle.values().each{ army -> army.sortForTargeting() }
    battle.values().each{ army ->
        army.groups.each{ g ->
            if(g.units <= 0) return
            Group best = null
            int bestD = 0
            battle.values().each{ enemyArmy ->
                if(enemyArmy.is(army)) return
                enemyArmy.groups.each{ e ->
                    if(e.units <= 0 || e.attacker) return
                    int d = g.damageTo(e)
                    if(d == 0) return
                    if(d > bestD ||
                       (d == bestD && (e.effectivePower() > (best?.effectivePower()?:-1))) ||
                       (d == bestD && e.effectivePower() == (best?.effectivePower()?:-1) && e.initiative > (best?.initiative?:-1))
                    ){
                        bestD = d
                        best = e
                    }
                }
            }
            if(best){
                g.target = best
                best.attacker = g
            }
        }
    }
}

def attackPhase = { battle ->
    def all = battle.values().collectMany{ it.groups }.findAll{ it.units>0 }
    all.sort{ a,b -> b.initiative <=> a.initiative }.each{ g ->
        if(g.units>0 && g.target && g.target.units>0){
            int dmg = g.damageTo(g.target)
            int killed = dmg / g.target.hitPoints
            if(killed>0){
                g.target.units -= Math.min(g.target.units, killed)
            }
        }
        if(g.target){
            g.target.attacker = null
            g.target = null
        }
    }
    battle.values().each{ it.groups = it.groups.findAll{ it.units>0 } }
}

def activeBattle = { battle -> battle.values().every{ it.alive() } }

def resultBattle = { battle ->
    int winner = 0
    int units = 0
    battle.each{ k, army ->
        if(army.alive()){
            winner = k
            units = army.groups.sum{ it.units }
        }
    }
    [winner, units]
}

int ArmyImmune = 1
int ArmyInfect = 2

int boost = 0
while(true){
    def battle = cloneBattle()
    battle[ArmyImmune].boost(boost)
    int prevTotal = -1
    boolean stalemate = false
    while(activeBattle(battle)){
        int before = totalUnits(battle)
        findTargets(battle)
        attackPhase(battle)
        if(totalUnits(battle) == before){ stalemate = true; break }
        if(totalUnits(battle) == prevTotal){ stalemate = true; break }
        prevTotal = totalUnits(battle)
    }
    if(!stalemate){
        def res = resultBattle(battle)
        if(res[0] == ArmyImmune){
            println res[1]
            System.exit(0)
        }
    }
    boost++
}