
#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");
    int steps;
    fscanf(fp, "%d", &steps);
    fclose(fp);

    int buffer[2018] = {0};
    int currentPosition = 0;

    for (int i = 1; i <= 2017; i++) {
        currentPosition = (currentPosition + steps) % i + 1;
        for (int j = i; j > currentPosition; j--) {
            buffer[j] = buffer[j - 1];
        }
        buffer[currentPosition] = i;
    }

    printf("%d\n", buffer[(currentPosition + 1) % 2018]);

    return 0;
}
