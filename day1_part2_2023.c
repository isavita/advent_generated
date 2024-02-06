
#include <stdio.h>
#include <string.h>

int findFirstAndLastDigit(char *line) {
    char *digits[] = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"};

    int firstDigit = 0, lastDigit = 0;
    for (int i = 0; i < strlen(line); i++) {
        char digitStr[2];
        digitStr[0] = line[i];
        digitStr[1] = '\0';
        
        if (digitStr[0] >= '0' && digitStr[0] <= '9') {
            if (firstDigit == 0) {
                firstDigit = digitStr[0] - '0';
            }
            lastDigit = digitStr[0] - '0';
        } else {
            for (int j = 0; j < 10; j++) {
                if (strncmp(&line[i], digits[j], strlen(digits[j])) == 0) {
                    if (firstDigit == 0) {
                        firstDigit = j;
                    }
                    lastDigit = j;
                    break;
                }
            }
        }
    }

    return 10*firstDigit + lastDigit;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        return 1;
    }

    int sum = 0;
    char line[100];
    while (fgets(line, 100, file) != NULL) {
        sum += findFirstAndLastDigit(line);
    }

    fclose(file);

    printf("%d\n", sum);

    return 0;
}
