
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

long long calculateWaysToWinLongRace(long long time, long long record) {
    long long waysToWin = 0;
    long long low = 0, high = time, mid;

    while (low <= high) {
        mid = low + (high - low) / 2;
        long long distance = mid * (time - mid);
        if (distance > record) {
           high = mid -1;
        } else {
           low = mid + 1;
        }
    }

    long long firstWin = low;

    low = 0;
    high = time;

     while (low <= high) {
        mid = low + (high - low) / 2;
        long long distance = mid * (time - mid);
        if (distance > record) {
           low = mid +1;
        } else {
            high = mid -1;
        }
    }

    long long lastWin = high;

    return lastWin - firstWin + 1;
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
    long long time = 0, distance = 0;

    while ((read = getline(&line, &len, file)) != -1) {
        if (line[0] == '\n' || line[0] == '\r' || line[0] == '\0' ) continue;
        char *colon = strchr(line, ':');
        if (colon == NULL) continue;
        colon++;
        char *num_str = (char *)malloc(strlen(colon) + 1);
        int j = 0;

        for(int i = 0; colon[i] != '\0' && colon[i] != '\n' && colon[i] != '\r'; i++) {
            if(isdigit(colon[i])) {
               num_str[j++] = colon[i];
            }
        }

        num_str[j] = '\0';


        if(time == 0) {
            time = atoll(num_str);
        } else {
            distance = atoll(num_str);
        }

         free(num_str);
    }

    free(line);
    fclose(file);

    long long waysToWin = calculateWaysToWinLongRace(time, distance);
    printf("%lld\n", waysToWin);

    return 0;
}
