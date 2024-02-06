
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int itemPriority(char item) {
    if (item >= 'a' && item <= 'z') {
        return (int)(item - 'a') + 1;
    }
    return (int)(item - 'A') + 27;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int sum = 0, groupLineCounter = 0;
    char line[1024];
    int groupItems[3][256] = {0}; // Assuming ASCII

    while (fgets(line, sizeof(line), file)) {
        int len = strlen(line);
        if (line[len - 1] == '\n') line[len - 1] = '\0'; // Remove newline

        for (int i = 0; line[i]; i++) {
            if (isalpha(line[i])) {
                groupItems[groupLineCounter][(unsigned char)line[i]]++;
            }
        }

        groupLineCounter++;
        if (groupLineCounter == 3) {
            for (int i = 0; i < 256; i++) {
                if (groupItems[0][i] > 0 && groupItems[1][i] > 0 && groupItems[2][i] > 0) {
                    sum += itemPriority(i);
                    break; // Since we need only one common item per group
                }
            }
            memset(groupItems, 0, sizeof(groupItems)); // Reset for next group
            groupLineCounter = 0;
        }
    }

    fclose(file);
    printf("%d\n", sum);
    return 0;
}
