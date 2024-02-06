
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        printf("File reading error\n");
        return 1;
    }

    int num_lines = 0;
    char line[256];
    while (fgets(line, sizeof(line), file)) {
        num_lines++;
    }

    int *offsets = (int *)malloc(num_lines * sizeof(int));
    fseek(file, 0, SEEK_SET);

    int i = 0;
    while (fgets(line, sizeof(line), file)) {
        offsets[i] = atoi(line);
        i++;
    }

    fclose(file);

    // Initialize Variables
    int index = 0;
    int steps = 0;

    // Navigate Maze
    while (index >= 0 && index < num_lines) {
        // Fetch the jump offset at the current index
        int jump = offsets[index];

        // Update Offset
        if (jump >= 3) {
            offsets[index]--;
        } else {
            offsets[index]++;
        }

        // Move to the new index
        index += jump;

        // Increment steps counter
        steps++;
    }

    // Output
    printf("%d\n", steps);

    free(offsets);
    return 0;
}
