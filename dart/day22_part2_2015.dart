import 'dart:io';
import 'dart:math';

class GameState {
  int playerHP, playerMana, bossHP, bossDamage;
  int shieldTimer, poisonTimer, rechargeTimer;
  int manaSpent;

  GameState({
    required this.playerHP,
    required this.playerMana,
    required this.bossHP,
    required this.bossDamage,
    this.shieldTimer = 0,
    this.poisonTimer = 0,
    this.rechargeTimer = 0,
    this.manaSpent = 0,
  });

  GameState clone() {
    return GameState(
      playerHP: playerHP,
      playerMana: playerMana,
      bossHP: bossHP,
      bossDamage: bossDamage,
      shieldTimer: shieldTimer,
      poisonTimer: poisonTimer,
      rechargeTimer: rechargeTimer,
      manaSpent: manaSpent,
    );
  }
}

int minManaToWin(GameState initialState) {
  int minMana = double.maxFinite.toInt();
  void simulate(GameState state, bool playerTurn) {
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
      state.playerHP--; // Lose 1 hit point at the start of player's turn
      if (state.playerHP <= 0) {
        return;
      }
    }

    // Apply effects
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
      int damage = state.bossDamage;
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

    // Cast spells
    if (state.playerMana >= 53) {
      GameState newState = state.clone();
      newState.playerMana -= 53;
      newState.manaSpent += 53;
      newState.bossHP -= 4;
      simulate(newState, false);
    }
    if (state.playerMana >= 73) {
      GameState newState = state.clone();
      newState.playerMana -= 73;
      newState.manaSpent += 73;
      newState.bossHP -= 2;
      newState.playerHP += 2;
      simulate(newState, false);
    }
    if (state.playerMana >= 113 && state.shieldTimer == 0) {
      GameState newState = state.clone();
      newState.playerMana -= 113;
      newState.manaSpent += 113;
      newState.shieldTimer = 6;
      simulate(newState, false);
    }
    if (state.playerMana >= 173 && state.poisonTimer == 0) {
      GameState newState = state.clone();
      newState.playerMana -= 173;
      newState.manaSpent += 173;
      newState.poisonTimer = 6;
      simulate(newState, false);
    }
    if (state.playerMana >= 229 && state.rechargeTimer == 0) {
      GameState newState = state.clone();
      newState.playerMana -= 229;
      newState.manaSpent += 229;
      newState.rechargeTimer = 5;
      simulate(newState, false);
    }
  }

  initialState.playerHP = 50;
  initialState.playerMana = 500;
  simulate(initialState, true);
  return minMana;
}

void main() async {
  final file = File('input.txt');
  final lines = await file.readAsLines();
  final bossHP = int.parse(lines[0].split(': ')[1]);
  final bossDamage = int.parse(lines[1].split(': ')[1]);

  final initialState = GameState(
    playerHP: 50,
    playerMana: 500,
    bossHP: bossHP,
    bossDamage: bossDamage,
  );
  print(minManaToWin(initialState));
}