
#include <stdio.h>

#define INVALID_NUMBER 14360655

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file\n");
        return 1;
    }

    int numbers[1000];
    int count = 0;
    int num;
    while (fscanf(file, "%d", &num) == 1) {
        numbers[count++] = num;
    }

    for (int i = 0; i < count; i++) {
        int sum = numbers[i];
        int min = numbers[i];
        int max = numbers[i];
        for (int j = i + 1; j < count; j++) {
            sum += numbers[j];
            if (numbers[j] < min) {
                min = numbers[j];
            }
            if (numbers[j] > max) {
                max = numbers[j];
            }
            if (sum == INVALID_NUMBER) {
                printf("%d\n", min + max);
                return 0;
            } else if (sum > INVALID_NUMBER) {
                break;
            }
        }
    }

    fclose(file);
    return 0;
}
