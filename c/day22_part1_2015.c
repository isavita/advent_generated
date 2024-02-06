
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

typedef struct GameState {
    int playerHP, playerMana, bossHP, bossDamage;
    int shieldTimer, poisonTimer, rechargeTimer;
    int manaSpent;
} GameState;

int minMana = INT_MAX;

void simulate(GameState state, int playerTurn) {
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
        simulate(state, 1);
        return;
    }

    // Player turn actions
    GameState newState;
    if (state.playerMana >= 53) {
        newState = state;
        newState.playerMana -= 53;
        newState.manaSpent += 53;
        newState.bossHP -= 4;
        simulate(newState, 0);
    }
    if (state.playerMana >= 73) {
        newState = state;
        newState.playerMana -= 73;
        newState.manaSpent += 73;
        newState.bossHP -= 2;
        newState.playerHP += 2;
        simulate(newState, 0);
    }
    if (state.playerMana >= 113 && state.shieldTimer == 0) {
        newState = state;
        newState.playerMana -= 113;
        newState.manaSpent += 113;
        newState.shieldTimer = 6;
        simulate(newState, 0);
    }
    if (state.playerMana >= 173 && state.poisonTimer == 0) {
        newState = state;
        newState.playerMana -= 173;
        newState.manaSpent += 173;
        newState.poisonTimer = 6;
        simulate(newState, 0);
    }
    if (state.playerMana >= 229 && state.rechargeTimer == 0) {
        newState = state;
        newState.playerMana -= 229;
        newState.manaSpent += 229;
        newState.rechargeTimer = 5;
        simulate(newState, 0);
    }
}

int minManaToWin(GameState initialState) {
    initialState.playerHP = 50;
    initialState.playerMana = 500;
    simulate(initialState, 1);
    return minMana;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    GameState initialState = {0};
    fscanf(file, "Hit Points: %d\n", &initialState.bossHP);
    fscanf(file, "Damage: %d\n", &initialState.bossDamage);
    fclose(file);

    printf("%d\n", minManaToWin(initialState));
    return 0;
}
