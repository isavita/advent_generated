
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int sumNumbers(char *input) {
    int sum = 0;
    int num = 0;
    int isNegative = 0;

    for (int i = 0; i < strlen(input); i++) {
        if (input[i] == '-') {
            isNegative = 1;
        } else if (input[i] >= '0' && input[i] <= '9') {
            num = num * 10 + (input[i] - '0');
        } else {
            if (isNegative) {
                num = -num;
                isNegative = 0;
            }
            sum += num;
            num = 0;
        }
    }

    return sum;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("Error opening file.\n");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long fileSize = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *input = (char *)malloc(fileSize + 1);
    fread(input, 1, fileSize, file);
    input[fileSize] = '\0';

    fclose(file);

    int result = sumNumbers(input);
    printf("%d\n", result);

    free(input);

    return 0;
}
