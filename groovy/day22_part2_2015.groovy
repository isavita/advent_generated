
def minManaToWin(initialState) {
    def minMana = Integer.MAX_VALUE
    def simulate

    simulate = { state, playerTurn ->
        if (state.manaSpent >= minMana) return
        if (state.bossHP <= 0) {
            minMana = state.manaSpent
            return
        }
        if (state.playerHP <= 0) return

        if (playerTurn) {
            state.playerHP--
            if (state.playerHP <= 0) return
        }

        if (state.shieldTimer > 0) state.shieldTimer--
        if (state.poisonTimer > 0) {
            state.bossHP -= 3
            state.poisonTimer--
        }
        if (state.rechargeTimer > 0) {
            state.playerMana += 101
            state.rechargeTimer--
        }

        if (!playerTurn) {
            def damage = state.bossDamage
            if (state.shieldTimer > 0) damage -= 7
            damage = Math.max(1, damage)
            state.playerHP -= damage
            simulate(state.clone(), true)
            return
        }

        if (state.playerMana >= 53) {
            def newState = state.clone()
            newState.playerMana -= 53
            newState.manaSpent += 53
            newState.bossHP -= 4
            simulate(newState, false)
        }
        if (state.playerMana >= 73) {
            def newState = state.clone()
            newState.playerMana -= 73
            newState.manaSpent += 73
            newState.bossHP -= 2
            newState.playerHP += 2
            simulate(newState, false)
        }
        if (state.playerMana >= 113 && state.shieldTimer == 0) {
            def newState = state.clone()
            newState.playerMana -= 113
            newState.manaSpent += 113
            newState.shieldTimer = 6
            simulate(newState, false)
        }
        if (state.playerMana >= 173 && state.poisonTimer == 0) {
            def newState = state.clone()
            newState.playerMana -= 173
            newState.manaSpent += 173
            newState.poisonTimer = 6
            simulate(newState, false)
        }
        if (state.playerMana >= 229 && state.rechargeTimer == 0) {
            def newState = state.clone()
            newState.playerMana -= 229
            newState.manaSpent += 229
            newState.rechargeTimer = 5
            simulate(newState, false)
        }
    }

    initialState.playerHP = 50
    initialState.playerMana = 500
    simulate(initialState, true)
    minMana
}

def file = new File("input.txt")
def lines = file.readLines()
def bossHP = lines[0].split(": ")[1] as int
def bossDamage = lines[1].split(": ")[1] as int

def initialState = [bossHP: bossHP, bossDamage: bossDamage, shieldTimer: 0, poisonTimer: 0, rechargeTimer: 0, manaSpent: 0]
println minManaToWin(initialState)
