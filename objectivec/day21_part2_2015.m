#import <Foundation/Foundation.h>

typedef struct {
    int cost;
    int damage;
    int armor;
} Item;

typedef struct {
    int hitPoints;
    int damage;
    int armor;
} Character;

void simulateCombat(Character player, Character boss, int *playerWins) {
    int playerHP = player.hitPoints;
    int bossHP = boss.hitPoints;

    while (true) {
        // Player attacks
        int damageToBoss = MAX(player.damage - boss.armor, 1);
        bossHP -= damageToBoss;
        if (bossHP <= 0) {
            *playerWins = 1;
            return;
        }

        // Boss attacks
        int damageToPlayer = MAX(boss.damage - player.armor, 1);
        playerHP -= damageToPlayer;
        if (playerHP <= 0) {
            *playerWins = 0;
            return;
        }
    }
}

int main(int argc, const char * argv[]) {
    @autoreleasepool {
        // Read input from file
        NSString *filePath = @"input.txt";
        NSString *fileContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
        NSArray *lines = [fileContent componentsSeparatedByString:@"\n"];

        // Parse boss stats
        int bossHP = [[lines[0] componentsSeparatedByString:@": "][1] intValue];
        int bossDamage = [[lines[1] componentsSeparatedByString:@": "][1] intValue];
        int bossArmor = [[lines[2] componentsSeparatedByString:@": "][1] intValue];

        Character boss = {bossHP, bossDamage, bossArmor};

        // Define shop items
        Item weapons[] = {
            {8, 4, 0}, {10, 5, 0}, {25, 6, 0}, {40, 7, 0}, {74, 8, 0}
        };

        Item armors[] = {
            {0, 0, 0}, {13, 0, 1}, {31, 0, 2}, {53, 0, 3}, {75, 0, 4}, {102, 0, 5}
        };

        Item rings[] = {
            {0, 0, 0}, {25, 1, 0}, {50, 2, 0}, {100, 3, 0}, {20, 0, 1}, {40, 0, 2}, {80, 0, 3}
        };

        int minCost = INT_MAX;
        int maxCost = 0;

        // Simulate all combinations
        for (int w = 0; w < 5; w++) {
            for (int a = 0; a < 6; a++) {
                for (int r1 = 0; r1 < 7; r1++) {
                    for (int r2 = r1 + 1; r2 < 7; r2++) {
                        int cost = weapons[w].cost + armors[a].cost + rings[r1].cost + rings[r2].cost;
                        int damage = weapons[w].damage + armors[a].damage + rings[r1].damage + rings[r2].damage;
                        int armor = weapons[w].armor + armors[a].armor + rings[r1].armor + rings[r2].armor;

                        Character player = {100, damage, armor};
                        int playerWins = 0;
                        simulateCombat(player, boss, &playerWins);

                        if (playerWins) {
                            minCost = MIN(minCost, cost);
                        } else {
                            maxCost = MAX(maxCost, cost);
                        }
                    }
                }
            }
        }

        // Print results
        printf("Least amount of gold to win: %d\n", minCost);
        printf("Most amount of gold to lose: %d\n", maxCost);
    }
    return 0;
}