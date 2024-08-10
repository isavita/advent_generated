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

int parseStat(NSString *line) {
    NSArray *parts = [line componentsSeparatedByString:@": "];
    return [parts[1] intValue];
}

BOOL playerWins(Character player, Character boss) {
    int playerDamage = MAX(1, player.damage - boss.armor);
    int bossDamage = MAX(1, boss.damage - player.armor);

    int playerTurns = (boss.hitPoints + playerDamage - 1) / playerDamage;
    int bossTurns = (player.hitPoints + bossDamage - 1) / bossDamage;

    return playerTurns <= bossTurns;
}

int main() {
    NSString *filePath = @"input.txt";
    NSError *error;
    NSString *fileContents = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
    if (error) {
        NSLog(@"%@", error);
        return 1;
    }

    NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
    Character boss;
    boss.hitPoints = parseStat(lines[0]);
    boss.damage = parseStat(lines[1]);
    boss.armor = parseStat(lines[2]);

    Item weapons[] = {
        {8, 4, 0},
        {10, 5, 0},
        {25, 6, 0},
        {40, 7, 0},
        {74, 8, 0}
    };

    Item armors[] = {
        {0, 0, 0},
        {13, 0, 1},
        {31, 0, 2},
        {53, 0, 3},
        {75, 0, 4},
        {102, 0, 5}
    };

    Item rings[] = {
        {0, 0, 0},
        {25, 1, 0},
        {50, 2, 0},
        {100, 3, 0},
        {20, 0, 1},
        {40, 0, 2},
        {80, 0, 3}
    };

    int minCost = INT_MAX;
    for (int i = 0; i < 5; i++) {
        for (int j = 0; j < 6; j++) {
            for (int k = 0; k < 7; k++) {
                for (int l = k + 1; l < 7; l++) {
                    Character player;
                    player.hitPoints = 100;
                    player.damage = weapons[i].damage + rings[k].damage + rings[l].damage;
                    player.armor = armors[j].armor + rings[k].armor + rings[l].armor;
                    int cost = weapons[i].cost + armors[j].cost + rings[k].cost + rings[l].cost;
                    if (playerWins(player, boss) && cost < minCost) {
                        minCost = cost;
                    }
                }
            }
        }
    }

    NSLog(@"%d", minCost);
    return 0;
}