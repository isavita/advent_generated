
#include <stdio.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    char lines[1000][1000];
    int count = 0;

    while (fgets(lines[count], 1000, file)) {
        lines[count][strcspn(lines[count], "\n")] = 0;
        count++;
    }

    for (int i = 0; i < count - 1; i++) {
        for (int j = i + 1; j < count; j++) {
            int diff = 0;
            for (int k = 0; k < strlen(lines[i]); k++) {
                if (lines[i][k] != lines[j][k]) {
                    diff++;
                    if (diff > 1) {
                        break;
                    }
                }
            }
            if (diff == 1) {
                char common[1000];
                int index = 0;
                for (int k = 0; k < strlen(lines[i]); k++) {
                    if (lines[i][k] == lines[j][k]) {
                        common[index++] = lines[i][k];
                    }
                }
                common[index] = '\0';
                printf("%s\n", common);
                return 0;
            }
        }
    }
    fclose(file);
    return 0;
}
