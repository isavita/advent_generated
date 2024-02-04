const fs = require('fs');

class GameState {
    constructor(playerHP, playerMana, bossHP, bossDamage, shieldTimer, poisonTimer, rechargeTimer, manaSpent) {
        this.playerHP = playerHP;
        this.playerMana = playerMana;
        this.bossHP = bossHP;
        this.bossDamage = bossDamage;
        this.shieldTimer = shieldTimer;
        this.poisonTimer = poisonTimer;
        this.rechargeTimer = rechargeTimer;
        this.manaSpent = manaSpent;
    }
}

function minManaToWin(initialState) {
    let minMana = Number.MAX_SAFE_INTEGER;
    
    const simulate = (state, playerTurn) => {
        if (state.manaSpent >= minMana) {
            return;
        }
        if (state.bossHP <= 0) {
            minMana = state.manaSpent;
            return;
        }
        if (state.playerHP <= 0) {
            return;
        }

        if (playerTurn) {
            state.playerHP--;
            if (state.playerHP <= 0) {
                return;
            }
        }

        if (state.shieldTimer > 0) {
            state.shieldTimer--;
        }
        if (state.poisonTimer > 0) {
            state.bossHP -= 3;
            state.poisonTimer--;
        }
        if (state.rechargeTimer > 0) {
            state.playerMana += 101;
            state.rechargeTimer--;
        }

        if (!playerTurn) {
            let damage = state.bossDamage;
            if (state.shieldTimer > 0) {
                damage -= 7;
            }
            if (damage < 1) {
                damage = 1;
            }
            state.playerHP -= damage;
            simulate(state, true);
            return;
        }

        if (state.playerMana >= 53) {
            let newState = new GameState(state.playerHP, state.playerMana - 53, state.bossHP - 4, state.bossDamage, state.shieldTimer, state.poisonTimer, state.rechargeTimer, state.manaSpent + 53);
            simulate(newState, false);
        }
        if (state.playerMana >= 73) {
            let newState = new GameState(state.playerHP + 2, state.playerMana - 73, state.bossHP - 2, state.bossDamage, state.shieldTimer, state.poisonTimer, state.rechargeTimer, state.manaSpent + 73);
            simulate(newState, false);
        }
        if (state.playerMana >= 113 && state.shieldTimer === 0) {
            let newState = new GameState(state.playerHP, state.playerMana - 113, state.bossHP, state.bossDamage, 6, state.poisonTimer, state.rechargeTimer, state.manaSpent + 113);
            simulate(newState, false);
        }
        if (state.playerMana >= 173 && state.poisonTimer === 0) {
            let newState = new GameState(state.playerHP, state.playerMana - 173, state.bossHP, state.bossDamage, state.shieldTimer, 6, state.rechargeTimer, state.manaSpent + 173);
            simulate(newState, false);
        }
        if (state.playerMana >= 229 && state.rechargeTimer === 0) {
            let newState = new GameState(state.playerHP, state.playerMana - 229, state.bossHP, state.bossDamage, state.shieldTimer, state.poisonTimer, 5, state.manaSpent + 229);
            simulate(newState, false);
        }
    }

    initialState.playerHP = 50;
    initialState.playerMana = 500;
    simulate(initialState, true);
    return minMana;
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const bossHP = parseInt(input[0].split(': ')[1]);
const bossDamage = parseInt(input[1].split(': ')[1]);

const initialState = new GameState(0, 0, bossHP, bossDamage, 0, 0, 0, 0);
console.log(minManaToWin(initialState));