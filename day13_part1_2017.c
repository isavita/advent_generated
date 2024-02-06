
#include <stdio.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    int depth, range, max_depth = 0, severity = 0;

    while (fscanf(file, "%d: %d\n", &depth, &range) != EOF) {
        if (depth > max_depth) {
            max_depth = depth;
        }
        if (depth % ((range - 1) * 2) == 0) {
            severity += depth * range;
        }
    }

    printf("%d\n", severity);

    fclose(file);
    return 0;
}
