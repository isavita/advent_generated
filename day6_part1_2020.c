#include <stdio.h>

int main() {
    FILE *fp = fopen("input.txt", "r");
    char line[100];
    int sum = 0;
    int count[26] = {0};

    while (fgets(line, sizeof(line), fp)) {
        if (line[0] == '\n') {
            for (int i = 0; i < 26; i++) {
                if (count[i] > 0) {
                    sum++;
                }
                count[i] = 0;
            }
        } else {
            for (int i = 0; line[i] != '\n'; i++) {
                count[line[i] - 'a']++;
            }
        }
    }

    for (int i = 0; i < 26; i++) {
        if (count[i] > 0) {
            sum++;
        }
    }

    printf("%d\n", sum);

    fclose(fp);
    return 0;
}