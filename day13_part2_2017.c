
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");

    int depth, range, max_depth = 0;
    while (fscanf(fp, "%d: %d\n", &depth, &range) != EOF) {
        if (depth > max_depth) {
            max_depth = depth;
        }
    }

    fseek(fp, 0, SEEK_SET);

    int *firewall = (int *)calloc(max_depth + 1, sizeof(int));
    while (fscanf(fp, "%d: %d\n", &depth, &range) != EOF) {
        firewall[depth] = range;
    }

    int delay = 0;
    int caught = 1;
    while (caught) {
        caught = 0;
        for (int i = 0; i <= max_depth; i++) {
            if (firewall[i] != 0) {
                if ((i + delay) % ((firewall[i] - 1) * 2) == 0) {
                    caught = 1;
                    delay++;
                    break;
                }
            }
        }
    }

    printf("%d\n", delay);

    fclose(fp);
    free(firewall);

    return 0;
}
