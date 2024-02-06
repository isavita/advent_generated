
#include <stdio.h>
#include <stdlib.h>

int abs(int x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

int* applyFFT(int* input, int length) {
    int basePattern[] = {0, 1, 0, -1};
    int* output = (int*)malloc(length * sizeof(int));
    for (int i = 0; i < length; i++) {
        int sum = 0;
        for (int j = 0; j < length; j++) {
            int patternValue = basePattern[((j+1)/(i+1))%4];
            sum += input[j] * patternValue;
        }
        output[i] = abs(sum % 10);
    }
    return output;
}

int main() {
    FILE* file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char input[1000];
    fgets(input, 1000, file);

    int length = 0;
    while (input[length] != '\0') {
        length++;
    }

    int* digits = (int*)malloc(length * sizeof(int));
    for (int i = 0; i < length; i++) {
        digits[i] = input[i] - '0';
    }

    for (int phase = 0; phase < 100; phase++) {
        digits = applyFFT(digits, length);
    }

    for (int i = 0; i < 8; i++) {
        printf("%d", digits[i]);
    }
    printf("\n");

    free(digits);
    fclose(file);
    return 0;
}
