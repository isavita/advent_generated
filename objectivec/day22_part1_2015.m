
#import <Foundation/Foundation.h>
#include <stdio.h>
#include <limits.h>

typedef struct {
    int playerHP, playerMana, bossHP, bossDamage;
    int shieldTimer, poisonTimer, rechargeTimer;
    int manaSpent;
} GameState;

int minMana = INT_MAX;

void simulate(GameState state, int playerTurn) {
    if (state.manaSpent >= minMana) return;
    if (state.bossHP <= 0) { minMana = state.manaSpent; return; }
    if (state.playerHP <= 0) return;

    if (state.shieldTimer) state.shieldTimer--;
    if (state.poisonTimer) { state.bossHP -= 3; state.poisonTimer--; }
    if (state.rechargeTimer) { state.playerMana += 101; state.rechargeTimer--; }

    if (!playerTurn) {
        int dmg = state.bossDamage;
        if (state.shieldTimer) dmg -= 7;
        if (dmg < 1) dmg = 1;
        state.playerHP -= dmg;
        simulate(state, 1);
        return;
    }

    GameState ns;
    if (state.playerMana >= 53) {
        ns = state;
        ns.playerMana -= 53;
        ns.manaSpent += 53;
        ns.bossHP -= 4;
        simulate(ns, 0);
    }
    if (state.playerMana >= 73) {
        ns = state;
        ns.playerMana -= 73;
        ns.manaSpent += 73;
        ns.bossHP -= 2;
        ns.playerHP += 2;
        simulate(ns, 0);
    }
    if (state.playerMana >= 113 && !state.shieldTimer) {
        ns = state;
        ns.playerMana -= 113;
        ns.manaSpent += 113;
        ns.shieldTimer = 6;
        simulate(ns, 0);
    }
    if (state.playerMana >= 173 && !state.poisonTimer) {
        ns = state;
        ns.playerMana -= 173;
        ns.manaSpent += 173;
        ns.poisonTimer = 6;
        simulate(ns, 0);
    }
    if (state.playerMana >= 229 && !state.rechargeTimer) {
        ns = state;
        ns.playerMana -= 229;
        ns.manaSpent += 229;
        ns.rechargeTimer = 5;
        simulate(ns, 0);
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        FILE *f = fopen("input.txt", "r");
        if (!f) return 1;
        GameState init = {0};
        fscanf(f, "Hit Points: %d\n", &init.bossHP);
        fscanf(f, "Damage: %d\n", &init.bossDamage);
        fclose(f);
        init.playerHP = 50;
        init.playerMana = 500;
        simulate(init, 1);
        printf("%d\n", minMana);
    }
    return 0;
}
