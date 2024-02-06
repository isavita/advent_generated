
#include <stdio.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    char line[1000];
    int count = 0;

    while (fgets(line, sizeof(line), file)) {
        int isTLS = 0;
        int inHypernet = 0;
        int abbaOutside = 0;
        int abbaInside = 0;
        
        for (int i = 0; i < strlen(line) - 3; i++) {
            if (line[i] == '[') {
                inHypernet = 1;
            } else if (line[i] == ']') {
                inHypernet = 0;
            } else if (line[i] != line[i + 1] && line[i] == line[i + 3] && line[i + 1] == line[i + 2]) {
                if (inHypernet) {
                    abbaInside = 1;
                } else {
                    abbaOutside = 1;
                }
            }
        }
        
        if (abbaOutside && !abbaInside) {
            count++;
        }
    }

    fclose(file);
    printf("%d\n", count);

    return 0;
}
