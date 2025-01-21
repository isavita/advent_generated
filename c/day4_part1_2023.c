
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    char line[1000];
    int totalPoints = 0;
    while (fgets(line, sizeof(line), file)) {
        char *start = strchr(line, ':');
        if (start == NULL) continue;
        start++;
        
        char *winning_str = strtok(start, "|");
        char *your_str = strtok(NULL, "|");
        
        int winningNumbers[50];
        int winningCount = 0;
        char *token = strtok(winning_str, " ");
        while (token != NULL) {
            winningNumbers[winningCount++] = atoi(token);
            token = strtok(NULL, " ");
        }

        int points = 0;
        token = strtok(your_str, " ");
        while (token != NULL) {
            int num = atoi(token);
            for (int i = 0; i < winningCount; i++) {
                if (winningNumbers[i] == num) {
                    points = points == 0 ? 1 : points * 2;
                    break;
                }
            }
            token = strtok(NULL, " ");
        }
        totalPoints += points;
    }

    fclose(file);
    printf("%d\n", totalPoints);
    return 0;
}
