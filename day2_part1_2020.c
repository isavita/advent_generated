
#include <stdio.h>
#include <string.h>

int main() {
    FILE *file = fopen("input.txt", "r");
    char line[50];
    int valid = 0;

    while (fgets(line, sizeof(line), file)) {
        int min, max;
        char letter, password[20];

        sscanf(line, "%d-%d %c: %s", &min, &max, &letter, password);

        int count = 0;
        for (int i = 0; i < strlen(password); i++) {
            if (password[i] == letter) {
                count++;
            }
        }

        if (count >= min && count <= max) {
            valid++;
        }
    }

    fclose(file);
    printf("%d\n", valid);

    return 0;
}
