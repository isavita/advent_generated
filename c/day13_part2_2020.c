
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

typedef long long ll;

ll extendedGCD(ll a, ll b, ll *x, ll *y) {
    if (a == 0) {
        *x = 0;
        *y = 1;
        return b;
    }
    ll x1, y1;
    ll gcd = extendedGCD(b % a, a, &x1, &y1);
    *x = y1 - (b / a) * x1;
    *y = x1;
    return gcd;
}

ll findEarliestTimestamp(ll *ids, ll *offsets, int n) {
    ll N = 1;
    for (int i = 0; i < n; i++) {
        N *= ids[i];
    }

    ll result = 0;
    for (int i = 0; i < n; i++) {
        ll ni = N / ids[i];
        ll xi, yi;
        extendedGCD(ni, ids[i], &xi, &yi);
        result += ((-offsets[i] % ids[i] + ids[i]) % ids[i] * xi % N * ni % N + N) % N;
    }
    return result % N;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    char *line = NULL;
    size_t len = 0;
    ssize_t read;

    if ((read = getline(&line, &len, file)) == -1) {
        perror("Error reading line");
        fclose(file);
        free(line);
        return 1;
    }

    if ((read = getline(&line, &len, file)) == -1) {
         perror("Error reading line");
        fclose(file);
        free(line);
        return 1;
    }
    
    char *token;
    ll ids[100];
    ll offsets[100];
    int count = 0;
    int offset_val = 0;

    token = strtok(line, ",");
    while (token != NULL) {
        if (strcmp(token, "x") != 0) {
            ids[count] = atoll(token);
            offsets[count] = offset_val;
            count++;
        }
         offset_val++;
        token = strtok(NULL, ",");
    }

    fclose(file);
    free(line);

    ll timestamp = findEarliestTimestamp(ids, offsets, count);
    printf("%lld\n", timestamp);

    return 0;
}
