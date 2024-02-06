
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    FILE* file = fopen("input.txt", "r");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    int k[14] = {0}, l[14] = {0}, m[14] = {0};
    char line[256];
    int i = 0, v;
    while (fgets(line, sizeof(line), file)) {
        switch (i % 18) {
            case 4:
                sscanf(line, "div z %d", &v);
                l[i / 18] = v;
                break;
            case 5:
                sscanf(line, "add x %d", &v);
                k[i / 18] = v;
                break;
            case 15:
                sscanf(line, "add y %d", &v);
                m[i / 18] = v;
                break;
        }
        i++;
    }
    fclose(file);

    int constraints[14][2] = {0};
    int stack[14], stackSize = 0;
    for (i = 0; i < 14; i++) {
        if (l[i] == 1) {
            stack[stackSize++] = i;
        } else if (l[i] == 26) {
            int pop = stack[--stackSize];
            constraints[pop][0] = i;
            constraints[pop][1] = m[pop] + k[i];
        }
    }

    int max[14] = {0};
    for (i = 0; i < 14; i++) {
        if (constraints[i][0] == 0 && constraints[i][1] == 0) continue;

        int vmax = 9;
        while (vmax + constraints[i][1] > 9) {
            vmax--;
        }
        max[i] = vmax;
        max[constraints[i][0]] = vmax + constraints[i][1];
    }

    long long n = 0;
    for (i = 0; i < 14; i++) {
        n = n * 10 + max[i];
    }

    printf("%lld\n", n);

    return 0;
}
