
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 1000 // Assuming a maximum of 1000 lines in input.txt

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char line[256];
    int x[MAX_LINES * 2]; // Assuming each operation can at most double the size
    int x_size = 1;
    x[0] = 1;

    while (fgets(line, sizeof(line), file)) {
        if (strncmp(line, "noop", 4) == 0) {
            x[x_size] = x[x_size - 1];
            x_size++;
        } else {
            int n;
            if (sscanf(line, "addx %d", &n) == 1) {
                x[x_size] = x[x_size - 1];
                x_size++;
                x[x_size] = x[x_size - 1] + n;
                x_size++;
            }
        }
    }
    fclose(file);

    int sum = 0;
    for (int i = 0; i < x_size; i++) {
        if ((i - 19) % 40 == 0) {
            sum += (i + 1) * x[i];
        }
    }

    printf("%d\n", sum);
    return 0;
}
