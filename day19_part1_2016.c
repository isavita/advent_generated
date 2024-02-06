
#include <stdio.h>

int main() {
    FILE *fp = fopen("input.txt", "r");
    int numElves;
    fscanf(fp, "%d", &numElves);
    fclose(fp);

    int i, winner = 0;
    for (i = 1; i <= numElves; i++) {
        winner = (winner + 2) % i;
    }

    printf("%d", winner + 1);

    return 0;
}
