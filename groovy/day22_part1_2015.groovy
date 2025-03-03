
import java.util.PriorityQueue

class Spell {
    String name
    int cost
    Closure<GameState> cast

    Spell(String name, int cost, Closure<GameState> cast) {
        this.name = name
        this.cost = cost
        this.cast = cast
    }
}

class GameState {
    int playerHp
    int playerMana
    int bossHp
    int bossDamage
    int armor
    Map<String, Integer> effects
    int manaSpent

    GameState(int playerHp, int playerMana, int bossHp, int bossDamage, int armor, Map<String, Integer> effects, int manaSpent) {
        this.playerHp = playerHp
        this.playerMana = playerMana
        this.bossHp = bossHp
        this.bossDamage = bossDamage
        this.armor = armor
        this.effects = effects
        this.manaSpent = manaSpent
    }

    GameState copy() {
        return new GameState(playerHp, playerMana, bossHp, bossDamage, armor, new HashMap<>(effects), manaSpent)
    }
}

def solve() {
    def bossHp
    def bossDamage
    new File('input.txt').eachLine { line ->
        if (line.startsWith('Hit Points')) {
            bossHp = line.split(': ')[1].toInteger()
        } else if (line.startsWith('Damage')) {
            bossDamage = line.split(': ')[1].toInteger()
        }
    }

    def spells = [
        new Spell("Magic Missile", 53, { GameState state ->
            state.bossHp -= 4
            return state
        }),
        new Spell("Drain", 73, { GameState state ->
            state.bossHp -= 2
            state.playerHp += 2
            return state
        }),
        new Spell("Shield", 113, { GameState state ->
            state.effects.put("Shield", 6)
            return state
        }),
        new Spell("Poison", 173, { GameState state ->
            state.effects.put("Poison", 6)
            return state
        }),
        new Spell("Recharge", 229, { GameState state ->
            state.effects.put("Recharge", 5)
            return state
        })
    ]

    def initialState = new GameState(50, 500, bossHp, bossDamage, 0, [:], 0)
    def minMana = Integer.MAX_VALUE

    def queue = new PriorityQueue<GameState>({ a, b -> a.manaSpent <=> b.manaSpent })
    queue.add(initialState)

    while (!queue.isEmpty()) {
        def state = queue.poll()

        if (state.manaSpent >= minMana) {
            continue
        }

        // Player turn
        state = applyEffects(state)
        if (state.bossHp <= 0) {
            minMana = Math.min(minMana, state.manaSpent)
            continue
        }

        if (state.playerHp <= 0) {
            continue
        }

        for (def spell in spells) {
            if (state.playerMana >= spell.cost && (!state.effects.containsKey(spell.name) || state.effects.get(spell.name) == 0)) {
                def newState = state.copy()
                newState.playerMana -= spell.cost
                newState.manaSpent += spell.cost

                newState = spell.cast(newState)

                if (newState.manaSpent >= minMana) {
                    continue
                }

                // Boss turn
                newState = applyEffects(newState)
                if (newState.bossHp <= 0) {
                    minMana = Math.min(minMana, newState.manaSpent)
                    continue
                }

                if (newState.playerHp <= 0) {
                    continue
                }

                newState.playerHp -= Math.max(1, newState.bossDamage - newState.armor)

                if (newState.playerHp <= 0) {
                    continue
                }

                queue.add(newState)
            }
        }
    }

    println minMana
}

def applyEffects(GameState state) {
    state.armor = 0
    if (state.effects.containsKey("Shield") && state.effects.get("Shield") > 0) {
        state.armor += 7
    }
    if (state.effects.containsKey("Poison") && state.effects.get("Poison") > 0) {
        state.bossHp -= 3
    }
    if (state.effects.containsKey("Recharge") && state.effects.get("Recharge") > 0) {
        state.playerMana += 101
    }

    def effectsToRemove = []
    state.effects.forEach { k, v ->
        state.effects.put(k, v - 1)
        if (v - 1 <= 0) {
            effectsToRemove << k
        }
    }
    effectsToRemove.each { state.effects.remove(it) }

    return state
}


static void main(String[] args) {
    solve()
}
