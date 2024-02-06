
#include <stdio.h>
#include <ctype.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int sum = 0;
    char line[100];

    while (fgets(line, sizeof(line), file)) {
        if (line[0] == '\n') {
            continue;
        }

        int firstDigit = -1, lastDigit = -1;
        for (int i = 0; line[i]; i++) {
            if (isdigit(line[i])) {
                if (firstDigit == -1) {
                    firstDigit = line[i] - '0';
                }
                lastDigit = line[i] - '0';
            }
        }

        if (firstDigit != -1 && lastDigit != -1) {
            int value = (firstDigit * 10) + lastDigit;
            sum += value;
        }
    }

    fclose(file);

    printf("%d\n", sum);
    return 0;
}
