#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int reg(char r) {
    return r - 'w';
}

int atoi_safe(const char *s) {
    return (int)strtol(s, NULL, 10);
}

void read_input(int *k, int *l, int *m) {
    FILE *file = fopen("input.txt", "r");
    char line[50];
    for (int i = 0; fgets(line, sizeof(line), file); i++) {
        int v;
        if (i % 18 == 4) sscanf(line, "div z %d", &v), l[i / 18] = v;
        if (i % 18 == 5) sscanf(line, "add x %d", &v), k[i / 18] = v;
        if (i % 18 == 15) sscanf(line, "add y %d", &v), m[i / 18] = v;
    }
    fclose(file);
}

int manual(const char *s) {
    int k[] = {11, 14, 10, 14, -8, 14, -11, 10, -6, -9, 12, -5, -4, -9};
    int l[] = {1, 1, 1, 1, 26, 1, 26, 1, 26, 26, 1, 26, 26, 26};
    int m[] = {7, 8, 16, 8, 3, 12, 1, 8, 8, 14, 4, 14, 15, 6};
    int w[14];
    for (int i = 0; i < 14; i++) w[i] = s[i] - '0';
    
    int z = 0;
    for (int i = 0; i < 14; i++) {
        int x = z % 26 + k[i];
        if (l[i] == 1) {
            z = z * 26 + w[i] + m[i];
        } else {
            z /= 26;
            if (x != w[i]) z = z * 26 + w[i] + m[i];
        }
    }
    return z;
}

int main() {
    int k[14], l[14], m[14];
    read_input(k, l, m);
    
    int constraints[14][2] = {0};
    int stack[14], stack_ptr = 0;
    
    for (int i = 0; i < 14; i++) {
        if (l[i] == 1) {
            stack[stack_ptr++] = i;
        } else if (l[i] == 26) {
            int pop = stack[--stack_ptr];
            constraints[pop][0] = i;
            constraints[pop][1] = m[pop] + k[i];
        }
    }
    
    int min[14] = {0};
    for (int i = 0; i < 14; i++) {
        if (constraints[i][0] == 0 && constraints[i][1] == 0) continue;
        int vmin = 1;
        while (vmin + constraints[i][1] < 1) vmin++;
        min[i] = vmin;
        min[constraints[i][0]] = vmin + constraints[i][1];
    }

    long long n = 0;
    for (int i = 0; i < 14; i++) {
        n = n * 10 + min[i];
    }
    
    printf("%lld\n", n);
    return 0;
}