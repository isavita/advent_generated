
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

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

int parseStat(char *line) {
    char *token = strchr(line, ':');
    if (token == NULL) {
        fprintf(stderr, "Invalid stat line: %s\n", line);
        exit(1);
    }
    return atoi(token + 2);
}

int playerWins(Character player, Character boss) {
    int playerDamage = (player.damage - boss.armor > 0) ? player.damage - boss.armor : 1;
    int bossDamage = (boss.damage - player.armor > 0) ? boss.damage - player.armor : 1;
    
    int playerTurns = (boss.hitPoints + playerDamage - 1) / playerDamage;
    int bossTurns = (player.hitPoints + bossDamage - 1) / bossDamage;

    return playerTurns <= bossTurns;
}

int max(int a, int b) {
    return (a > b) ? a : b;
}

int main() {
    FILE *fp = fopen("input.txt", "r");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[256];
    Character boss;

    fgets(line, sizeof(line), fp);
    boss.hitPoints = parseStat(line);
    fgets(line, sizeof(line), fp);
    boss.damage = parseStat(line);
    fgets(line, sizeof(line), fp);
    boss.armor = parseStat(line);
    fclose(fp);

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


    int maxCost = 0;
    for (int w = 0; w < 5; w++) {
        for (int a = 0; a < 6; a++) {
            for (int ri = 0; ri < 7; ri++) {
                for (int rj = ri + 1; rj < 7; rj++) {
                    Character player = {100, weapons[w].damage, armors[a].armor};
                    player.damage += rings[ri].damage + rings[rj].damage;
                    player.armor += rings[ri].armor + rings[rj].armor;

                    int cost = weapons[w].cost + armors[a].cost + rings[ri].cost + rings[rj].cost;

                     if (!playerWins(player, boss) && cost > maxCost) {
                        maxCost = cost;
                    }
                }
            }
        }
    }

    printf("%d\n", maxCost);
    return 0;
}
