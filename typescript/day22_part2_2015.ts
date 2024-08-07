interface GameState {
  playerHP: number;
  playerMana: number;
  bossHP: number;
  bossDamage: number;
  shieldTimer: number;
  poisonTimer: number;
  rechargeTimer: number;
  manaSpent: number;
}

function minManaToWin(initialState: GameState): number {
  let minMana = Number.MAX_SAFE_INTEGER;

  function simulate(state: GameState, playerTurn: boolean) {
    if (state.manaSpent >= minMana) return;
    if (state.bossHP <= 0) {
      minMana = state.manaSpent;
      return;
    }
    if (state.playerHP <= 0) return;

    if (playerTurn) {
      state.playerHP--;
      if (state.playerHP <= 0) return;
    }

    if (state.shieldTimer > 0) state.shieldTimer--;
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
      if (state.shieldTimer > 0) damage -= 7;
      if (damage < 1) damage = 1;
      state.playerHP -= damage;
      simulate(state, true);
      return;
    }

    if (state.playerMana >= 53) {
      const newState = { ...state };
      newState.playerMana -= 53;
      newState.manaSpent += 53;
      newState.bossHP -= 4;
      simulate(newState, false);
    }
    if (state.playerMana >= 73) {
      const newState = { ...state };
      newState.playerMana -= 73;
      newState.manaSpent += 73;
      newState.bossHP -= 2;
      newState.playerHP += 2;
      simulate(newState, false);
    }
    if (state.playerMana >= 113 && state.shieldTimer === 0) {
      const newState = { ...state };
      newState.playerMana -= 113;
      newState.manaSpent += 113;
      newState.shieldTimer = 6;
      simulate(newState, false);
    }
    if (state.playerMana >= 173 && state.poisonTimer === 0) {
      const newState = { ...state };
      newState.playerMana -= 173;
      newState.manaSpent += 173;
      newState.poisonTimer = 6;
      simulate(newState, false);
    }
    if (state.playerMana >= 229 && state.rechargeTimer === 0) {
      const newState = { ...state };
      newState.playerMana -= 229;
      newState.manaSpent += 229;
      newState.rechargeTimer = 5;
      simulate(newState, false);
    }
  }

  const initialStateWithPlayer = { 
    ...initialState, 
    playerHP: 50, 
    playerMana: 500, 
    shieldTimer: 0, 
    poisonTimer: 0, 
    rechargeTimer: 0 
  };
  simulate(initialStateWithPlayer, true);
  return minMana;
}

function main() {
  const fs = require('fs');
  const input = fs.readFileSync('input.txt', 'utf8');
  const lines = input.split('\n');
  const bossHP = parseInt(lines[0].split(': ')[1]);
  const bossDamage = parseInt(lines[1].split(': ')[1]);

  const initialState: GameState = { 
    bossHP, 
    bossDamage, 
    manaSpent: 0, 
    playerHP: 0, 
    playerMana: 0, 
    shieldTimer: 0, 
    poisonTimer: 0, 
    rechargeTimer: 0 
  };
  console.log(minManaToWin(initialState));
}

main();