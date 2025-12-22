
#import <Foundation/Foundation.h>
#import <limits.h>

typedef struct {
    int playerHP;
    int playerMana;
    int bossHP;
    int bossDamage;
    int shieldTimer;
    int poisonTimer;
    int rechargeTimer;
    int manaSpent;
} GameState;

static int minMana = INT_MAX;

void simulate(GameState state, BOOL playerTurn) {
    if (state.manaSpent >= minMana) return;
    if (state.bossHP <= 0) { minMana = state.manaSpent; return; }
    if (state.playerHP <= 0) return;
    if (playerTurn) {
        state.playerHP--;
        if (state.playerHP <= 0) return;
    }
    if (state.shieldTimer > 0) state.shieldTimer--;
    if (state.poisonTimer > 0) { state.bossHP -= 3; state.poisonTimer--; }
    if (state.rechargeTimer > 0) { state.playerMana += 101; state.rechargeTimer--; }
    if (!playerTurn) {
        int dmg = state.bossDamage;
        if (state.shieldTimer > 0) dmg -= 7;
        if (dmg < 1) dmg = 1;
        state.playerHP -= dmg;
        simulate(state, YES);
        return;
    }
    if (state.playerMana >= 53) {
        GameState ns = state;
        ns.playerMana -= 53;
        ns.manaSpent += 53;
        ns.bossHP -= 4;
        simulate(ns, NO);
    }
    if (state.playerMana >= 73) {
        GameState ns = state;
        ns.playerMana -= 73;
        ns.manaSpent += 73;
        ns.bossHP -= 2;
        ns.playerHP += 2;
        simulate(ns, NO);
    }
    if (state.playerMana >= 113 && state.shieldTimer == 0) {
        GameState ns = state;
        ns.playerMana -= 113;
        ns.manaSpent += 113;
        ns.shieldTimer = 6;
        simulate(ns, NO);
    }
    if (state.playerMana >= 173 && state.poisonTimer == 0) {
        GameState ns = state;
        ns.playerMana -= 173;
        ns.manaSpent += 173;
        ns.poisonTimer = 6;
        simulate(ns, NO);
    }
    if (state.playerMana >= 229 && state.rechargeTimer == 0) {
        GameState ns = state;
        ns.playerMana -= 229;
        ns.manaSpent += 229;
        ns.rechargeTimer = 5;
        simulate(ns, NO);
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSString *content = [NSString stringWithContentsOfFile:@"input.txt" encoding:NSUTF8StringEncoding error:nil];
        NSArray<NSString *> *lines = [content componentsSeparatedByString:@"\n"];
        GameState init = {0};
        for (NSString *line in lines) {
            if ([line hasPrefix:@"Hit Points:"]) {
                init.bossHP = [[line componentsSeparatedByString:@": "][1] intValue];
            } else if ([line hasPrefix:@"Damage:"]) {
                init.bossDamage = [[line componentsSeparatedByString:@": "][1] intValue];
            }
        }
        init.playerHP = 50;
        init.playerMana = 500;
        simulate(init, YES);
        printf("%d\n", minMana);
    }
    return 0;
}
