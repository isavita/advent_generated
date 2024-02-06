
#include <stdio.h>
#include <stdlib.h>

#define PREAMBLE_LENGTH 25
#define INPUT_SIZE 1000

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    if (fp == NULL) {
        printf("Error opening file\n");
        exit(1);
    }

    int numbers[INPUT_SIZE];
    int index = 0;
    while (fscanf(fp, "%d", &numbers[index]) != EOF) {
        index++;
    }

    fclose(fp);

    int invalid_number;
    for (int i = PREAMBLE_LENGTH; i < index; i++) {
        int valid = 0;
        for (int j = i - PREAMBLE_LENGTH; j < i; j++) {
            for (int k = j + 1; k < i; k++) {
                if (numbers[j] + numbers[k] == numbers[i]) {
                    valid = 1;
                    break;
                }
            }
            if (valid) {
                break;
            }
        }
        if (!valid) {
            invalid_number = numbers[i];
            break;
        }
    }

    printf("%d\n", invalid_number);

    return 0;
}
