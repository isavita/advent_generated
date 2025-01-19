
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

int calculateWaysToWin(long long time, long long record) {
    int waysToWin = 0;
    long long low = 1, high = time - 1;
    while (low <= high) {
       long long mid = low + (high - low) / 2;
       long long dist = mid * (time - mid);
       if(dist > record){
           high = mid -1;
       }else{
           low = mid + 1;
       }
    }
    return time - 2* low + 1;
}

int main() {
    FILE *file = fopen("input.txt", "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    long long times[10];
    long long distances[10];
    int timeCount = 0;
    int distCount = 0;

    char *line = NULL;
    size_t len = 0;
    ssize_t read;
    int line_num = 0;
    while ((read = getline(&line, &len, file)) != -1) {
        char *token = strtok(line, " ");
         while (token != NULL) {
            long long num;
            if (sscanf(token, "%lld", &num) == 1) {
                if(line_num == 0){
                 times[timeCount++] = num;
                } else{
                    distances[distCount++] = num;
                }
            }
             token = strtok(NULL, " ");
        }
        line_num++;
    }
    free(line);
    fclose(file);

    long long totalWays = 1;
    for (int i = 0; i < timeCount; i++) {
        totalWays *= calculateWaysToWin(times[i], distances[i]);
    }

    printf("%lld\n", totalWays);
    return 0;
}
