
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file");
        return 1;
    }

    char line[100];
    char *parts[15];

    int mfcsam[10][2] = {{3, 0}, {7, 0}, {2, 0}, {3, 0}, {0, 0}, {0, 0}, {5, 0}, {3, 0}, {2, 0}, {1, 0}};

    while (fgets(line, sizeof(line), file)) {
        char *token = strtok(line, " ");
        int i = 0;
        while (token != NULL) {
            parts[i++] = token;
            token = strtok(NULL, " ");
        }
        parts[1][strlen(parts[1])-1] = '\0';
        char *sueNumber = parts[1];

        int matches = 1;
        for (int j = 2; j < i; j += 2) {
            parts[j][strlen(parts[j])-1] = '\0';
            int count = atoi(parts[j+1]);
            int index;
            for (index = 0; index < 10; index++) {
                if (strcmp(parts[j], "cats") == 0 || strcmp(parts[j], "trees") == 0) {
                    if (strcmp(parts[j], "cats") == 0) index = 1;
                    if (strcmp(parts[j], "trees") == 0) index = 7;
                    break;
                }
                if (strcmp(parts[j], "pomeranians") == 0 || strcmp(parts[j], "goldfish") == 0) {
                    if (strcmp(parts[j], "pomeranians") == 0) index = 3;
                    if (strcmp(parts[j], "goldfish") == 0) index = 6;
                    break;
                }
                if (strcmp(parts[j], "akitas") == 0 || strcmp(parts[j], "vizslas") == 0) {
                    if (strcmp(parts[j], "akitas") == 0) index = 4;
                    if (strcmp(parts[j], "vizslas") == 0) index = 5;
                    break;
                }
                if (strcmp(parts[j], "children") == 0 || strcmp(parts[j], "cars") == 0 || strcmp(parts[j], "perfumes") == 0) {
                    if (strcmp(parts[j], "children") == 0) index = 0;
                    if (strcmp(parts[j], "cars") == 0) index = 8;
                    if (strcmp(parts[j], "perfumes") == 0) index = 9;
                    break;
                }
            }
            if (mfcsam[index][0] != count) {
                matches = 0;
                break;
            }
        }

        if (matches) {
            printf("%s\n", sueNumber);
            break;
        }
    }

    fclose(file);

    return 0;
}
