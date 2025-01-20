
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

int parseStat(const char *line) {
    return atoi(strchr(line, ':') + 2);
}

int max(int a, int b) {
    return a > b ? a : b;
}

int playerWins(Character player, Character boss) {
    int playerDamage = max(1, player.damage - boss.armor);
    int bossDamage = max(1, boss.damage - player.armor);

    int playerTurns = (boss.hitPoints + playerDamage - 1) / playerDamage;
    int bossTurns = (player.hitPoints + bossDamage - 1) / bossDamage;

    return playerTurns <= bossTurns;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    char line[256];
    fgets(line, sizeof(line), file);
    Character boss = {parseStat(line), 0, 0};
    fgets(line, sizeof(line), file);
    boss.damage = parseStat(line);
    fgets(line, sizeof(line), file);
    boss.armor = parseStat(line);
    fclose(file);

    Item weapons[] = {{8, 4, 0}, {10, 5, 0}, {25, 6, 0}, {40, 7, 0}, {74, 8, 0}};
    Item armors[] = {{0, 0, 0}, {13, 0, 1}, {31, 0, 2}, {53, 0, 3}, {75, 0, 4}, {102, 0, 5}};
    Item rings[] = {{0, 0, 0}, {25, 1, 0}, {50, 2, 0}, {100, 3, 0}, {20, 0, 1}, {40, 0, 2}, {80, 0, 3}};

    int minCost = __INT_MAX__;
    for (int w = 0; w < 5; w++) {
        for (int a = 0; a < 6; a++) {
            for (int ri = 0; ri < 7; ri++) {
                for (int rj = ri + 1; rj < 7; rj++) {
                    Character player = {100, weapons[w].damage, armors[a].armor};
                    player.damage += rings[ri].damage + rings[rj].damage;
                    player.armor += rings[ri].armor + rings[rj].armor;
                    int cost = weapons[w].cost + armors[a].cost + rings[ri].cost + rings[rj].cost;
                    if (playerWins(player, boss) && cost < minCost) {
                        minCost = cost;
                    }
                }
            }
        }
    }

    printf("%d\n", minCost);
    return 0;
}
