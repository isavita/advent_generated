#include <stdio.h>

int main() {
    FILE *fp;
    fp = fopen("input.txt", "r");

    int validCount1 = 0;
    int validCount2 = 0;

    while (!feof(fp)) {
        int min, max;
        char letter, password[50];
        fscanf(fp, "%d-%d %c: %s", &min, &max, &letter, password);

        int count = 0;
        for (int i = 0; password[i] != '\0'; i++) {
            if (password[i] == letter) {
                count++;
            }
        }

        if (count >= min && count <= max) {
            validCount1++;
        }

        if ((password[min - 1] == letter && password[max - 1] != letter) || (password[min - 1] != letter && password[max - 1] == letter)) {
            validCount2++;
        }
    }

    printf("%d\n", validCount1);
    printf("%d\n", validCount2);

    fclose(fp);

    return 0;
}